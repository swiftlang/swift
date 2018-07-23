//===--- LinearLifetimeChecker.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file defines a linear lifetime checker for SIL. A value with a linear
/// lifetime is defined as a value that is guaranteed to be consuming exactly
/// once along any path through the program and has a guarantee that all
/// non-consuming uses and the initial value are joint-postdominated by the set
/// of consuming uses.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-linear-lifetime-checker"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::ownership;

//===----------------------------------------------------------------------===//
//                                Declarations
//===----------------------------------------------------------------------===//

namespace {

using BrPropUserAndBlockPair = std::pair<BranchPropagatedUser, SILBasicBlock *>;

struct State {
  /// The value that we are checking.
  SILValue value;

  /// The behavior of the checker when we detect an error. Can either be
  /// returning false, returning false with a message emitted to stderr, or an
  /// assert.
  ErrorBehaviorKind errorBehavior;

  /// The blocks that we have already visited.
  SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks;

  /// The set of blocks with consuming uses.
  SmallPtrSet<SILBasicBlock *, 8> blocksWithConsumingUses;

  /// The set of blocks with non-consuming uses and the associated
  /// non-consuming use SILInstruction.
  llvm::SmallDenseMap<SILBasicBlock *, BranchPropagatedUser, 8>
      blocksWithNonConsumingUses;

  /// The worklist that we use when performing our block dataflow.
  SmallVector<SILBasicBlock *, 32> worklist;

  /// A list of successor blocks that we must visit by the time the algorithm
  /// terminates.
  SmallPtrSet<SILBasicBlock *, 8> successorBlocksThatMustBeVisited;

  State(SILValue value, SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks,
        ErrorBehaviorKind errorBehavior)
      : value(value), errorBehavior(errorBehavior),
        visitedBlocks(visitedBlocks) {}

  void initializeAllNonConsumingUses(
      ArrayRef<BranchPropagatedUser> nonConsumingUsers);
  bool initializeAllConsumingUses(
      ArrayRef<BranchPropagatedUser> consumingUsers,
      SmallVectorImpl<BrPropUserAndBlockPair> &predsToAddToWorklist);

  /// Initializes state for a consuming use. Returns true if we have not yet
  /// seen a consuming use in the same block yet. Returns false if detect such a
  /// condition so users know that a use-after-free was detected.
  bool initializeConsumingUse(BranchPropagatedUser consumingUser,
                              SILBasicBlock *userBlock);

  /// Returns true if the given block contains a non-consuming use that is
  /// strictly later in the block than a consuming use. If all
  /// non-consuming uses are before the consuming use, the block is
  /// removed from the blocksWithNonConsumingUses map to show that the uses
  /// were found to properly be post-dominated by a consuming use.
  bool checkForSameBlockUseAfterFree(BranchPropagatedUser consumingUser,
                                     SILBasicBlock *userBlock);

  /// Once we have marked all of our producing blocks.
  bool checkPredsForDoubleConsume(BranchPropagatedUser consumingUser,
                                  SILBasicBlock *userBlock);
  bool checkPredsForDoubleConsume(SILBasicBlock *userBlock);

  /// Once we have setup all of our consuming/non-consuming blocks and have
  /// validated that all intra-block dataflow is safe, perform the inter-block
  /// dataflow.
  bool performDataflow(DeadEndBlocks &deBlocks);

  /// After we have performed the dataflow, check the end state of our dataflow
  /// for validity. If this is a linear typed value, return true. Return false
  /// otherwise.
  bool checkDataflowEndState(DeadEndBlocks &deBlocks);

  /// Depending on our initialization, either return false or call Func and
  /// throw an error.
  bool handleError(llvm::function_ref<void()> &&messagePrinterFunc) const {
    if (errorBehavior.shouldPrintMessage()) {
      messagePrinterFunc();
    }

    if (errorBehavior.shouldReturnFalse()) {
      return false;
    }

    assert(errorBehavior.shouldAssert() && "At this point, we should assert");
    llvm_unreachable("triggering standard assertion failure routine");
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                      Non Consuming Use Initialization
//===----------------------------------------------------------------------===//

void State::initializeAllNonConsumingUses(
    ArrayRef<BranchPropagatedUser> nonConsumingUsers) {
  for (BranchPropagatedUser user : nonConsumingUsers) {
    auto *userBlock = user.getParent();
    // First try to associate User with User->getParent().
    auto result =
        blocksWithNonConsumingUses.insert(std::make_pair(userBlock, user));

    // If the insertion succeeds, then we know that there is no more work to
    // be done, so process the next use.
    if (result.second)
      continue;

    // If the insertion fails, then we have at least two non-consuming
    // uses in the same block. Since we are performing a liveness type of
    // dataflow, we only need the last non-consuming use to show that all
    // consuming uses post dominate both.
    //
    // We begin by checking if the first use is a cond_br use from the previous
    // block. In such a case, we always use the already stored value and
    // continue.
    if (user.isCondBranchUser()) {
      continue;
    }

    // Then, we check if Use is after Result.first->second in the use list. If
    // Use is not later, then we wish to keep the already mapped value, not use,
    // so continue.
    if (std::find_if(result.first->second.getIterator(), userBlock->end(),
                     [&user](const SILInstruction &i) -> bool {
                       return user == &i;
                     }) == userBlock->end()) {
      continue;
    }

    // At this point, we know that user is later in the Block than
    // result.first->second, so store user instead.
    result.first->second = user;
  }
}

//===----------------------------------------------------------------------===//
//                        Consuming Use Initialization
//===----------------------------------------------------------------------===//

bool State::initializeAllConsumingUses(
    ArrayRef<BranchPropagatedUser> consumingUses,
    SmallVectorImpl<std::pair<BranchPropagatedUser, SILBasicBlock *>>
        &predsToAddToWorklist) {
  for (BranchPropagatedUser user : consumingUses) {
    SILBasicBlock *userBlock = user.getParent();

    // First initialize our state for the consuming user. This returns false if
    // we found another consuming instruction associated with userBlock and true
    // if we successfully associated user with userBlock.
    if (!initializeConsumingUse(user, userBlock)) {
      // We already handled the error.
      return handleError([] {});
    }

    // Then check if the given block has a use after free.
    if (checkForSameBlockUseAfterFree(user, userBlock)) {
      // We already handled the error.
      return handleError([] {});
    }

    // If this user is in the same block as the value, do not visit
    // predecessors. We must be extra tolerant here since we allow for
    // unreachable code.
    if (userBlock == value->getParentBlock())
      continue;

    // Then for each predecessor of this block...
    for (auto *pred : userBlock->getPredecessorBlocks()) {
      // If this block is not a block that we have already put on the list, add
      // it to the worklist.
      predsToAddToWorklist.push_back({user, pred});
    }
  }

  return true;
}

bool State::initializeConsumingUse(BranchPropagatedUser consumingUser,
                                   SILBasicBlock *userBlock) {
  // Map this user to the block. If we already have a value for the block, then
  // we have a double consume and need to fail.
  if (blocksWithConsumingUses.insert(userBlock).second)
    return true;

  return handleError([&] {
    llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n"
                 << "Found over consume?!\n"
                 << "Value: " << *value << "User: " << *consumingUser
                 << "Block: bb" << userBlock->getDebugID() << "\n\n";
  });
}

bool State::checkForSameBlockUseAfterFree(BranchPropagatedUser consumingUser,
                                          SILBasicBlock *userBlock) {
  // If we do not have any consuming uses in the same block as our
  // consuming user, then we can not have a same block use-after-free.
  auto iter = blocksWithNonConsumingUses.find(userBlock);
  if (iter == blocksWithNonConsumingUses.end())
    return false;

  BranchPropagatedUser nonConsumingUser = iter->second;

  // Make sure that the non-consuming use is before the consuming
  // use. Otherwise, we have a use after free.

  // First check if our consuming user is a cond_br. In such a case, we
  // always consider the non-consuming use to be a use after free since
  // the cond branch user is in a previous block. So just bail early.
  if (consumingUser.isCondBranchUser()) {
    return !handleError([&]() {
      llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n"
                   << "Found use after free?!\n"
                   << "Value: " << *value
                   << "Consuming User: " << *consumingUser
                   << "Non Consuming User: " << *iter->second << "Block: bb"
                   << userBlock->getDebugID() << "\n\n";
    });
  }

  // Ok. At this point, we know that our consuming user is not a cond branch
  // user. Check if our non-consuming user is. In such a case, we know that our
  // non-consuming user is properly post-dominated so we can ignore the
  // consuming use. and continue.
  if (nonConsumingUser.isCondBranchUser()) {
    blocksWithNonConsumingUses.erase(iter);
    return false;
  }

  // Otherwise, we know that both of our users are non-cond branch users and
  // thus must be instructions in the given block. Make sure that the non
  // consuming user is strictly before the consuming user.
  if (std::find_if(consumingUser.getIterator(), userBlock->end(),
                   [&nonConsumingUser](const SILInstruction &i) -> bool {
                     return nonConsumingUser == &i;
                   }) != userBlock->end()) {
    return !handleError([&] {
      llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n"
                   << "Found use after free?!\n"
                   << "Value: " << *value
                   << "Consuming User: " << *consumingUser
                   << "Non Consuming User: " << *iter->second << "Block: bb"
                   << userBlock->getDebugID() << "\n\n";
    });
  }

  // Erase the use since we know that it is properly joint post-dominated.
  blocksWithNonConsumingUses.erase(iter);
  return false;
}

bool State::checkPredsForDoubleConsume(BranchPropagatedUser consumingUser,
                                       SILBasicBlock *userBlock) {
  if (!blocksWithConsumingUses.count(userBlock))
    return false;

  return !handleError([&] {
    llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n"
                 << "Found over consume?!\n"
                 << "Value: " << *value << "User: " << *consumingUser
                 << "Block: bb" << userBlock->getDebugID() << "\n\n";
  });
}

bool State::checkPredsForDoubleConsume(SILBasicBlock *userBlock) {
  if (!blocksWithConsumingUses.count(userBlock))
    return false;

  return !handleError([&] {
    llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n"
                 << "Found over consume?!\n"
                 << "Value: " << *value << "Block: bb"
                 << userBlock->getDebugID() << "\n\n";
  });
}

//===----------------------------------------------------------------------===//
//                                  Dataflow
//===----------------------------------------------------------------------===//

bool State::performDataflow(DeadEndBlocks &deBlocks) {
  LLVM_DEBUG(llvm::dbgs() << "    Beginning to check dataflow constraints\n");
  // Until the worklist is empty...
  while (!worklist.empty()) {
    // Grab the next block to visit.
    SILBasicBlock *block = worklist.pop_back_val();
    LLVM_DEBUG(llvm::dbgs() << "    Visiting Block: bb" << block->getDebugID()
                            << '\n');

    // Since the block is on our worklist, we know already that it is not a
    // block with lifetime ending uses, due to the invariants of our loop.

    // First remove BB from the SuccessorBlocksThatMustBeVisited list. This
    // ensures that when the algorithm terminates, we know that BB was not the
    // beginning of a non-covered path to the exit.
    successorBlocksThatMustBeVisited.erase(block);

    // Then remove BB from BlocksWithNonLifetimeEndingUses so we know that
    // this block was properly joint post-dominated by our lifetime ending
    // users.
    blocksWithNonConsumingUses.erase(block);

    // Ok, now we know that we do not have an overconsume. If this block does
    // not end in a no return function, we need to update our state for our
    // successors to make sure by the end of the traversal we visit them.
    //
    // We must consider such no-return blocks since we may be running during
    // SILGen before NoReturn folding has run.
    for (auto *succBlock : block->getSuccessorBlocks()) {
      // If we already visited the successor, there is nothing to do since we
      // already visited the successor.
      if (visitedBlocks.count(succBlock))
        continue;

      // Then check if the successor is a transitively unreachable block. In
      // such a case, we ignore it since we are going to leak along that path.
      if (deBlocks.isDeadEnd(succBlock))
        continue;

      // Otherwise, add the successor to our SuccessorBlocksThatMustBeVisited
      // set to ensure that we assert if we do not visit it by the end of the
      // algorithm.
      successorBlocksThatMustBeVisited.insert(succBlock);
    }

    // If we are at the dominating block of our walk, continue. There is nothing
    // further to do since we do not want to visit the predecessors of our
    // dominating block. On the other hand, we do want to add its successors to
    // the successorBlocksThatMustBeVisited set.
    if (block == value->getParentBlock())
      continue;

    // Then for each predecessor of this block:
    //
    // 1. If we have visited the predecessor already, then it is not a block
    // with lifetime ending uses. If it is a block with uses, then we have a
    // double release... so assert. If not, we continue.
    //
    // 2. We add the predecessor to the worklist if we have not visited it yet.
    for (auto *predBlock : block->getPredecessorBlocks()) {
      if (checkPredsForDoubleConsume(predBlock)) {
        return handleError([] {});
      }

      if (visitedBlocks.count(predBlock)) {
        continue;
      }

      visitedBlocks.insert(predBlock);
      worklist.push_back(predBlock);
    }
  }

  return true;
}

bool State::checkDataflowEndState(DeadEndBlocks &deBlocks) {
  // Make sure that we visited all successor blocks that we needed to visit to
  // make sure we didn't leak.
  if (!successorBlocksThatMustBeVisited.empty()) {
    return handleError([&] {
      llvm::errs()
          << "Function: '" << value->getFunction()->getName() << "'\n"
          << "Error! Found a leak due to a consuming post-dominance failure!\n"
          << "    Value: " << *value << "    Post Dominating Failure Blocks:\n";
      for (auto *succBlock : successorBlocksThatMustBeVisited) {
        llvm::errs() << "        bb" << succBlock->getDebugID();
      }
      llvm::errs() << '\n';
    });
  }

  // Make sure that we do not have any lifetime ending uses left to visit that
  // are not transitively unreachable blocks.... so return early.
  if (blocksWithNonConsumingUses.empty()) {
    return true;
  }

  // If we do have remaining blocks, then these non lifetime ending uses must be
  // outside of our "alive" blocks implying a use-after free.
  for (auto &pair : blocksWithNonConsumingUses) {
    if (deBlocks.isDeadEnd(pair.first)) {
      continue;
    }

    return handleError([&] {
      llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n"
                   << "Found use after free due to unvisited non lifetime "
                      "ending uses?!\n"
                   << "Value: " << *value << "    Remaining Users:\n";
      for (auto &pair : blocksWithNonConsumingUses) {
        llvm::errs() << "User:" << *pair.second << "Block: bb"
                     << pair.first->getDebugID() << "\n";
      }
      llvm::errs() << "\n";
    });
  }

  // If all of our remaining blocks were dead uses, then return true. We are
  // good.
  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

bool swift::valueHasLinearLifetime(
    SILValue value, ArrayRef<BranchPropagatedUser> consumingUses,
    ArrayRef<BranchPropagatedUser> nonConsumingUses,
    SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks, DeadEndBlocks &deBlocks,
    ErrorBehaviorKind errorBehavior) {
  assert(!consumingUses.empty() && "Must have at least one consuming user?!");

  State state(value, visitedBlocks, errorBehavior);

  // First add our non-consuming uses and their blocks to the
  // blocksWithNonConsumingUses map. While we do this, if we have multiple uses
  // in the same block, we only accept the last use since from a liveness
  // perspective that is all we care about.
  state.initializeAllNonConsumingUses(nonConsumingUses);

  // Then, we go through each one of our consuming users performing the
  // following operation:
  //
  // 1. Verifying that no two consuming users are in the same block. This
  // is accomplished by adding the user blocks to the blocksWithConsumingUsers
  // list. This avoids double consumes.
  //
  // 2. Verifying that no predecessor is a block with a consuming use. The
  // reason why this is necessary is because we wish to not add elements to the
  // worklist twice. Thus we want to check if we have already visited a
  // predecessor.
  SmallVector<BrPropUserAndBlockPair, 32> predsToAddToWorklist;
  state.initializeAllConsumingUses(consumingUses, predsToAddToWorklist);

  // If we have a singular consuming use and it is in the same block as value's
  // def, we bail early. Any use-after-frees due to non-consuming uses would
  // have been detected by initializing our consuming uses. So we are done.
  if (consumingUses.size() == 1 &&
      consumingUses[0].getParent() == value->getParentBlock()) {
    return true;
  }

  // Ok, we may have multiple consuming uses. Add the user block of each of our
  // consuming users to the visited list since we do not want them to be added
  // to the successors to visit set.
  for (const auto &i : consumingUses) {
    state.visitedBlocks.insert(i.getParent());
  }

  // Now that we have marked all of our producing blocks, we go through our
  // predsToAddToWorklist list and add our preds, making sure that none of these
  // preds are in blocksWithConsumingUses. This is important so that we do not
  // need to re-process.
  for (auto pair : predsToAddToWorklist) {
    BranchPropagatedUser user = pair.first;
    SILBasicBlock *predBlock = pair.second;

    // Make sure that the predecessor is not in our blocksWithConsumingUses
    // list.
    if (state.checkPredsForDoubleConsume(user, predBlock)) {
      return state.handleError([] {});
    }

    if (!state.visitedBlocks.insert(predBlock).second)
      continue;
    state.worklist.push_back(predBlock);
  }

  // Now that our algorithm is completely prepared, run the
  // dataflow... If we find a failure, return false.
  if (!state.performDataflow(deBlocks))
    return false;

  // ...and then check that the end state shows that we have a valid linear
  // typed value.
  return state.checkDataflowEndState(deBlocks);
}
