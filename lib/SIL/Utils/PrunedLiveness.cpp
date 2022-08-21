//===--- PrunedLiveness.cpp - Compute liveness from selected uses ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/PrunedLiveness.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"

using namespace swift;

/// Mark blocks live during a reverse CFG traversal from one specific block
/// containing a user.
void PrunedLiveBlocks::computeUseBlockLiveness(SILBasicBlock *userBB,
                                               unsigned startBitNo,
                                               unsigned endBitNo) {
  // If, we are visiting this block, then it is not already LiveOut. Mark it
  // LiveWithin to indicate a liveness boundary within the block.
  markBlockLive(userBB, startBitNo, endBitNo, LiveWithin);

  SmallVector<IsLive, 8> predLivenessInfo;
  BasicBlockWorklist worklist(userBB->getFunction());
  worklist.push(userBB);

  while (auto *block = worklist.pop()) {
    // The popped `bb` is live; now mark all its predecessors LiveOut.
    //
    // Traversal terminates at any previously visited block, including the
    // blocks initialized as definition blocks.
    for (auto *predBlock : block->getPredecessorBlocks()) {
      SWIFT_DEFER { predLivenessInfo.clear(); };
      getBlockLiveness(predBlock, startBitNo, endBitNo, predLivenessInfo);
      for (unsigned i : indices(predLivenessInfo)) {
        switch (predLivenessInfo[i]) {
        case Dead:
          worklist.pushIfNotVisited(predBlock);
          LLVM_FALLTHROUGH;
        case LiveWithin:
          markBlockLive(predBlock, startBitNo, endBitNo, LiveOut);
          break;
        case LiveOut:
          break;
        }
      }
    }
  }
}

/// Update the current def's liveness based on one specific use instruction.
///
/// Return the updated liveness of the \p use block (LiveOut or LiveWithin).
///
/// Terminators are not live out of the block.
void PrunedLiveBlocks::updateForUse(
    SILInstruction *user, unsigned startBitNo, unsigned endBitNo,
    SmallVectorImpl<IsLive> &resultingLivenessInfo) {
  SWIFT_ASSERT_ONLY(seenUse = true);

  auto *bb = user->getParent();
  getBlockLiveness(bb, startBitNo, endBitNo, resultingLivenessInfo);

  for (auto isLive : resultingLivenessInfo) {
    switch (isLive) {
    case LiveOut:
    case LiveWithin:
      continue;
    case Dead: {
      // This use block has not yet been marked live. Mark it and its
      // predecessor blocks live.
      computeUseBlockLiveness(bb, startBitNo, endBitNo);
      resultingLivenessInfo.clear();
      return getBlockLiveness(bb, startBitNo, endBitNo, resultingLivenessInfo);
    }
    }
    llvm_unreachable("covered switch");
  }
}

//===----------------------------------------------------------------------===//
//                            MARK: PrunedLiveness
//===----------------------------------------------------------------------===//

void PrunedLiveness::updateForUse(SILInstruction *user, bool lifetimeEnding) {
  auto useBlockLive = liveBlocks.updateForUse(user, 0);
  // Record all uses of blocks on the liveness boundary. For blocks marked
  // LiveWithin, the boundary is considered to be the last use in the block.
  //
  // FIXME: Why is nonLifetimeEndingUsesInLiveOut inside PrunedLiveness, and
  // what does it mean? Blocks may transition to LiveOut later. Or they may
  // already be LiveOut from a previous use. After computing liveness, clients
  // should check uses that are in PrunedLivenessBoundary.
  if (!lifetimeEnding && useBlockLive == PrunedLiveBlocks::LiveOut) {
    if (nonLifetimeEndingUsesInLiveOut)
      nonLifetimeEndingUsesInLiveOut->insert(user);
    return;
  }
  // Note that a user may use the current value from multiple operands. If any
  // of the uses are non-lifetime-ending, then we must consider the user
  // itself non-lifetime-ending; it cannot be a final destroy point because
  // the value of the non-lifetime-ending operand must be kept alive until the
  // end of the user. Consider a call that takes the same value using
  // different conventions:
  //
  //   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  //
  // This call is not considered the end of %val's lifetime. The @owned
  // argument must be copied.
  auto iterAndSuccess = users.insert({user, lifetimeEnding});
  if (!iterAndSuccess.second)
    iterAndSuccess.first->second &= lifetimeEnding;
}

bool PrunedLiveness::updateForBorrowingOperand(Operand *op) {
  assert(op->getOperandOwnership() == OperandOwnership::Borrow);

  // A nested borrow scope is considered a use-point at each scope ending
  // instruction.
  //
  // TODO: Handle reborrowed copies by considering the extended borrow
  // scope. Temporarily bail-out on reborrows because we can't handle uses
  // that aren't dominated by currentDef.
  if (!BorrowingOperand(op).visitScopeEndingUses([this](Operand *end) {
        if (end->getOperandOwnership() == OperandOwnership::Reborrow) {
          return false;
        }
        updateForUse(end->getUser(), /*lifetimeEnding*/ false);
        return true;
      })) {
    return false;
  }
  return true;
}

void PrunedLiveness::extendAcrossLiveness(PrunedLiveness &otherLivesness) {
  // update this liveness for all the interesting users in otherLiveness.
  for (std::pair<SILInstruction *, bool> userAndEnd : otherLivesness.users) {
    updateForUse(userAndEnd.first, userAndEnd.second);
  }
}

bool PrunedLiveness::isWithinBoundaryHelper(SILInstruction *inst,
                                            SILValue def) const {
  SILBasicBlock *block = inst->getParent();

  /// Returns true if \p inst is before \p def in this block.
  auto foundInBlockBeforeDef = [](SILInstruction *inst, SILBasicBlock *block,
                                  SILValue def) {
    if (!def || def->getParentBlock() != block) {
      return false;
    }
    auto *defInst = def->getDefiningInstruction();
    if (!defInst) {
      return false;
    }
    // Check if instruction is before the definition
    for (SILInstruction &it :
         make_range(block->begin(), defInst->getIterator())) {
      if (&it == inst) {
        return true;
      }
    }
    return false;
  };

  switch (getBlockLiveness(block)) {
  case PrunedLiveBlocks::Dead:
    return false;
  case PrunedLiveBlocks::LiveOut:
    return !foundInBlockBeforeDef(inst, block, def);
  case PrunedLiveBlocks::LiveWithin:
    if (foundInBlockBeforeDef(inst, block, def)) {
      return false;
    }
    // The boundary is within this block. This instruction is before the
    // boundary iff any interesting uses occur after it.
    for (SILInstruction &it :
         make_range(std::next(inst->getIterator()), block->end())) {
      switch (isInterestingUser(&it)) {
      case PrunedLiveness::NonUser:
        break;
      case PrunedLiveness::NonLifetimeEndingUse:
      case PrunedLiveness::LifetimeEndingUse:
        return true;
      }
    }
    return false;
  }
}

bool PrunedLiveness::isWithinBoundary(SILInstruction *inst) const {
  return isWithinBoundaryHelper(inst, /*def*/ SILValue());
}

bool PrunedLiveness::isWithinBoundaryOfDef(SILInstruction *inst,
                                           SILValue def) const {
  return isWithinBoundaryHelper(inst, def);
}

bool PrunedLiveness::areUsesWithinBoundaryHelper(
    ArrayRef<Operand *> uses, SILValue def,
    DeadEndBlocks *deadEndBlocks) const {
  auto checkDeadEnd = [deadEndBlocks](SILInstruction *inst) {
    return deadEndBlocks && deadEndBlocks->isDeadEnd(inst->getParent());
  };

  for (auto *use : uses) {
    auto *user = use->getUser();
    if (!isWithinBoundaryHelper(user, def) && !checkDeadEnd(user))
      return false;
  }
  return true;
}

bool PrunedLiveness::areUsesWithinBoundary(ArrayRef<Operand *> uses,
                                           DeadEndBlocks *deadEndBlocks) const {
  return areUsesWithinBoundaryHelper(uses, SILValue(), deadEndBlocks);
}

bool PrunedLiveness::areUsesWithinBoundaryOfDef(
    ArrayRef<Operand *> uses, SILValue def,
    DeadEndBlocks *deadEndBlocks) const {
  return areUsesWithinBoundaryHelper(uses, def, deadEndBlocks);
}

bool PrunedLiveness::areUsesOutsideBoundaryHelper(
    ArrayRef<Operand *> uses, SILValue def,
    DeadEndBlocks *deadEndBlocks) const {
  auto checkDeadEnd = [deadEndBlocks](SILInstruction *inst) {
    return deadEndBlocks && deadEndBlocks->isDeadEnd(inst->getParent());
  };

  for (auto *use : uses) {
    auto *user = use->getUser();
    if (isWithinBoundaryHelper(user, def) || checkDeadEnd(user))
      return false;
  }
  return true;
}

bool PrunedLiveness::areUsesOutsideBoundary(
    ArrayRef<Operand *> uses, DeadEndBlocks *deadEndBlocks) const {
  return areUsesOutsideBoundaryHelper(uses, SILValue(), deadEndBlocks);
}

bool PrunedLiveness::areUsesOutsideBoundaryOfDef(
    ArrayRef<Operand *> uses, SILValue def,
    DeadEndBlocks *deadEndBlocks) const {
  return areUsesOutsideBoundaryHelper(uses, def, deadEndBlocks);
}

// An SSA def meets all the criteria for pruned liveness--def dominates all
// uses with no holes in the liverange. The lifetime-ending uses are also
// recorded--destroy_value or end_borrow. However destroy_values may not
// jointly-post dominate if dead-end blocks are present.
void PrunedLiveness::computeSSALiveness(SILValue def) {
  initializeDefBlock(def->getParentBlock());
  for (Operand *use : def->getUses()) {
    updateForUse(use->getUser(), use->isLifetimeEnding());
  }
}

void PrunedLivenessBoundary::visitInsertionPoints(
    llvm::function_ref<void(SILBasicBlock::iterator insertPt)> visitor,
    DeadEndBlocks *deBlocks) {
  for (SILInstruction *user : lastUsers) {
    if (!isa<TermInst>(user)) {
      visitor(std::next(user->getIterator()));
      continue;
    }
    auto *predBB = user->getParent();
    for (SILBasicBlock *succ : predBB->getSuccessors()) {
      if (deBlocks && deBlocks->isDeadEnd(succ))
        continue;

      assert(succ->getSinglePredecessorBlock() == predBB);
      visitor(succ->begin());
    }
  }
  for (SILBasicBlock *edge : boundaryEdges) {
    if (deBlocks && deBlocks->isDeadEnd(edge))
      continue;

    visitor(edge->begin());
  }
}

// Use \p liveness to find the last use in \p bb and add it to \p
// boundary.lastUsers.
static void findLastUserInBlock(SILBasicBlock *bb,
                                PrunedLivenessBoundary &boundary,
                                const PrunedLiveness &liveness) {
  for (auto instIter = bb->rbegin(), endIter = bb->rend(); instIter != endIter;
       ++instIter) {
    auto *inst = &*instIter;
    if (liveness.isInterestingUser(inst) == PrunedLiveness::NonUser)
      continue;

    boundary.lastUsers.push_back(inst);
    return;
  }
  llvm_unreachable("No user in LiveWithin block");
}

void PrunedLivenessBoundary::compute(const PrunedLiveness &liveness) {
  for (SILBasicBlock *bb : liveness.getDiscoveredBlocks()) {
    // Process each block that has not been visited and is not LiveOut.
    switch (liveness.getBlockLiveness(bb)) {
    case PrunedLiveBlocks::LiveOut:
      for (SILBasicBlock *succBB : bb->getSuccessors()) {
        if (liveness.getBlockLiveness(succBB) == PrunedLiveBlocks::Dead) {
          boundaryEdges.push_back(succBB);
        }
      }
      break;
    case PrunedLiveBlocks::LiveWithin: {
      // The liveness boundary is inside this block. Insert a final destroy
      // inside the block if it doesn't already have one.
      findLastUserInBlock(bb, *this, liveness);
      break;
    }
    case PrunedLiveBlocks::Dead:
      llvm_unreachable("All discovered blocks must be live");
    }
  }
}

void PrunedLivenessBoundary::compute(const PrunedLiveness &liveness,
                                     ArrayRef<SILBasicBlock *> postDomBlocks) {
  if (postDomBlocks.empty())
    return; // all paths must be dead-ends or infinite loops

  BasicBlockWorklist blockWorklist(postDomBlocks[0]->getParent());

  // Visit each post-dominating block as the starting point for a
  // backward CFG traversal.
  for (auto *bb : postDomBlocks) {
    blockWorklist.push(bb);
  }
  while (auto *bb = blockWorklist.pop()) {
    // Process each block that has not been visited and is not LiveOut.
    switch (liveness.getBlockLiveness(bb)) {
    case PrunedLiveBlocks::LiveOut:
      // A lifetimeEndBlock may be determined to be LiveOut after analyzing
      // the extended liveness. It is irrelevant for finding the boundary.
      break;
    case PrunedLiveBlocks::LiveWithin: {
      // The liveness boundary is inside this block. Insert a final destroy
      // inside the block if it doesn't already have one.
      findLastUserInBlock(bb, *this, liveness);
      break;
    }
    case PrunedLiveBlocks::Dead:
      // Continue searching upward to find the pruned liveness boundary.
      for (auto *predBB : bb->getPredecessorBlocks()) {
        if (liveness.getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
          boundaryEdges.push_back(bb);
        } else {
          blockWorklist.pushIfNotVisited(predBB);
        }
      }
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
//                       Field Sensitive PrunedLiveness
//===----------------------------------------------------------------------===//

// We can only analyze components of structs whose storage is fully accessible
// from Swift.
static StructDecl *getFullyReferenceableStruct(SILType ktypeTy) {
  auto structDecl = ktypeTy.getStructOrBoundGenericStruct();
  if (!structDecl || structDecl->hasUnreferenceableStorage())
    return nullptr;
  return structDecl;
}

TypeSubElementCount::TypeSubElementCount(SILType type, SILModule &mod,
                                         TypeExpansionContext context)
    : number(1) {
  if (auto tupleType = type.getAs<TupleType>()) {
    unsigned numElements = 0;
    for (auto index : indices(tupleType.getElementTypes()))
      numElements +=
          TypeSubElementCount(type.getTupleElementType(index), mod, context);
    number = numElements;
    return;
  }

  if (auto *structDecl = getFullyReferenceableStruct(type)) {
    unsigned numElements = 0;
    for (auto *fieldDecl : structDecl->getStoredProperties())
      numElements += TypeSubElementCount(
          type.getFieldType(fieldDecl, mod, context), mod, context);
    number = numElements;
    return;
  }

  // If this isn't a tuple or struct, it is a single element. This was our
  // default value, so we can just return.
}

Optional<SubElementNumber>
SubElementNumber::compute(SILValue projectionDerivedFromRoot,
                          SILValue rootAddress) {
  unsigned finalSubElementNumber = 0;
  SILModule &mod = *rootAddress->getModule();

  while (1) {
    // If we got to the root, we're done.
    if (rootAddress == projectionDerivedFromRoot)
      return {SubElementNumber(finalSubElementNumber)};

    if (auto *pbi = dyn_cast<ProjectBoxInst>(projectionDerivedFromRoot)) {
      projectionDerivedFromRoot = pbi->getOperand();
      continue;
    }

    if (auto *bai = dyn_cast<BeginAccessInst>(projectionDerivedFromRoot)) {
      projectionDerivedFromRoot = bai->getSource();
      continue;
    }

    if (auto *teai =
            dyn_cast<TupleElementAddrInst>(projectionDerivedFromRoot)) {
      SILType tupleType = teai->getOperand()->getType();

      // Keep track of what subelement is being referenced.
      for (unsigned i : range(teai->getFieldIndex())) {
        finalSubElementNumber += TypeSubElementCount(
            tupleType.getTupleElementType(i), mod,
            TypeExpansionContext(*rootAddress->getFunction()));
      }
      projectionDerivedFromRoot = teai->getOperand();
      continue;
    }

    if (auto *seai =
            dyn_cast<StructElementAddrInst>(projectionDerivedFromRoot)) {
      SILType type = seai->getOperand()->getType();

      // Keep track of what subelement is being referenced.
      StructDecl *structDecl = seai->getStructDecl();
      for (auto *fieldDecl : structDecl->getStoredProperties()) {
        if (fieldDecl == seai->getField())
          break;
        auto context = TypeExpansionContext(*rootAddress->getFunction());
        finalSubElementNumber += TypeSubElementCount(
            type.getFieldType(fieldDecl, mod, context), mod, context);
      }

      projectionDerivedFromRoot = seai->getOperand();
      continue;
    }

    // This fails when we visit unchecked_take_enum_data_addr. We should just
    // add support for enums.
#ifndef NDEBUG
    if (!isa<InitExistentialAddrInst>(projectionDerivedFromRoot)) {
      llvm::errs() << "Unknown access path instruction!\n";
      llvm::errs() << "Value: " << *projectionDerivedFromRoot;
      llvm_unreachable("standard error");
    }
#endif
    // Cannot promote loads and stores from within an existential projection.
    return None;
  }
}

void FieldSensitiveAddressPrunedLiveness::updateForUse(
    SILInstruction *user, TypeTreeLeafTypeRange range, bool lifetimeEnding) {
  SmallVector<PrunedLiveBlocks::IsLive, 8> resultingLiveness;
  liveBlocks.updateForUse(user, range.startEltOffset, range.endEltOffset,
                          resultingLiveness);

  // Note that a user may use the current value from multiple operands. If any
  // of the uses are non-lifetime-ending, then we must consider the user
  // itself non-lifetime-ending; it cannot be a final destroy point because
  // the value of the non-lifetime-ending operand must be kept alive until the
  // end of the user. Consider a call that takes the same value using
  // different conventions:
  //
  //   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  //
  // This call is not considered the end of %val's lifetime. The @owned
  // argument must be copied.
  auto iterAndSuccess =
      users.insert({user, InterestingUser(range, lifetimeEnding)});
  if (!iterAndSuccess.second)
    iterAndSuccess.first->second &= lifetimeEnding;
}

void FieldSensitiveAddressPrunedLiveness::isWithinBoundary(
    SILInstruction *inst, SmallBitVector &outVector) const {
  SILBasicBlock *block = inst->getParent();

  SmallVector<PrunedLiveBlocks::IsLive, 8> fieldLiveness;
  getBlockLiveness(block, fieldLiveness);
  outVector.resize(fieldLiveness.size());

  for (auto pair : llvm::enumerate(fieldLiveness)) {
    auto isLive = pair.value();
    unsigned subEltNumber = pair.index();
    switch (isLive) {
    case PrunedLiveBlocks::Dead:
      outVector[subEltNumber] = false;
      continue;
    case PrunedLiveBlocks::LiveOut:
      outVector[subEltNumber] = true;
      continue;
    case PrunedLiveBlocks::LiveWithin:
      // The boundary is within this block. This instruction is before the
      // boundary iff any interesting uses occur after it.
      bool foundValue = false;
      for (SILInstruction &it :
           make_range(std::next(inst->getIterator()), block->end())) {
        auto interestingUser = isInterestingUser(&it);
        switch (interestingUser.first) {
        case FieldSensitiveAddressPrunedLiveness::NonUser:
          break;
        case FieldSensitiveAddressPrunedLiveness::NonLifetimeEndingUse:
        case FieldSensitiveAddressPrunedLiveness::LifetimeEndingUse:
          // Check the overlap in between the sub element number and
          // interestingUser.second. If we don't overlap, just break. We aren't
          // effected by this.
          //
          // TODO: Hoist this out! We should only be visited blocks like this
          // once!
          if (!interestingUser.second->contains(subEltNumber))
            break;
          outVector[subEltNumber] = true;
          foundValue = true;
          break;
        }
      }
      if (foundValue)
        continue;
      outVector[subEltNumber] = false;
    }
  }
}

// Use \p liveness to find the last use in \p bb and add it to \p
// boundary.lastUsers.
void FieldSensitiveAddressPrunedLivenessBoundary::findLastUserInBlock(
    SILBasicBlock *bb, FieldSensitiveAddressPrunedLivenessBoundary &boundary,
    const FieldSensitiveAddressPrunedLiveness &liveness,
    unsigned subElementNumber) {
  // TODO: We should move this loop into the caller and only visit a block once
  // for each sub-element of a type.
  for (auto &inst : llvm::reverse(*bb)) {
    auto pair = liveness.isInterestingUser(&inst);
    if (pair.first == FieldSensitiveAddressPrunedLiveness::NonUser)
      continue;

    // Do an intersection in between the range associated with this address and
    // the sub-element number we are checking for.
    auto &range = *pair.second;
    if (!range.contains(subElementNumber))
      continue;
    boundary.lastUsers.push_back({&inst, range});
    return;
  }
  llvm_unreachable("No user in LiveWithin block");
}

void FieldSensitiveAddressPrunedLivenessBoundary::compute(
    const FieldSensitiveAddressPrunedLiveness &liveness) {
  using IsLive = PrunedLiveBlocks::IsLive;
  SmallVector<IsLive, 8> perSubElementblockLivenessInfo;
  SmallVector<IsLive, 8> boundaryBlockLiveness;

  for (SILBasicBlock *bb : liveness.getDiscoveredBlocks()) {
    SWIFT_DEFER { perSubElementblockLivenessInfo.clear(); };

    // Process each block that has not been visited and is not LiveOut.
    liveness.getBlockLiveness(bb, perSubElementblockLivenessInfo);

    // TODO: We should do this for all sub-element LiveWithin at the same time
    // so that we can avoid iterating over the block multiple times.
    for (auto pair : llvm::enumerate(perSubElementblockLivenessInfo)) {
      switch (pair.value()) {
      case PrunedLiveBlocks::LiveOut:
        for (SILBasicBlock *succBB : bb->getSuccessors()) {
          liveness.getBlockLiveness(succBB, boundaryBlockLiveness);
          if (llvm::all_of(boundaryBlockLiveness, [](IsLive isDead) {
                return isDead == PrunedLiveBlocks::Dead;
              })) {
            boundaryEdges.push_back(succBB);
          }
        }
        break;
      case PrunedLiveBlocks::LiveWithin: {
        // The liveness boundary is inside this block. Find the last user. This
        // is where we would insert a destroy to end the values lifetime for the
        // specific subelementnumber
        findLastUserInBlock(bb, *this, liveness, pair.index());
        break;
      }
      case PrunedLiveBlocks::Dead:
        break;
      }
    }
  }
}
