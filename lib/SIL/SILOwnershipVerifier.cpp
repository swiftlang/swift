//===--- SILOwnershipVerifier.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-ownership-verifier"

#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/TransformArrayRef.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILOpenedArchetypesTracker.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include <algorithm>

using namespace swift;
using namespace swift::ownership;

// This is an option to put the SILOwnershipVerifier in testing mode. This
// causes the following:
//
// 1. Instead of printing an error message and aborting, the verifier will print
// the message and continue. This allows for FileCheck testing of the verifier.
//
// 2. SILInstruction::verifyOperandOwnership() is disabled. This is used for
// verification in SILBuilder. This causes errors to be printed twice, once when
// we build the IR and a second time when we perform a full verification of the
// IR. For testing purposes, we just want the later.
llvm::cl::opt<bool> IsSILOwnershipVerifierTestingEnabled(
    "sil-ownership-verifier-enable-testing",
    llvm::cl::desc("Put the sil ownership verifier in testing mode. See "
                   "comment in SILOwnershipVerifier.cpp above option for more "
                   "information."));

/// This is an option to turn off ownership verification on a specific file. We
/// still emit code as if we are in ownership mode, but we do not verify. This
/// is useful for temporarily turning off verification on tests.
static llvm::cl::opt<bool>
    DisableOwnershipVerification("disable-sil-ownership-verification");

//===----------------------------------------------------------------------===//
//                         SILValueOwnershipChecker
//===----------------------------------------------------------------------===//

namespace {

// TODO: This class uses a bunch of global state like variables. It should be
// refactored into a large state object that is used by functions.
class SILValueOwnershipChecker {
  /// The result of performing the check.
  llvm::Optional<bool> result;

  /// The module that we are in.
  SILModule &mod;

  /// A cache of dead-end basic blocks that we use to determine if we can
  /// ignore "leaks".
  DeadEndBlocks &deadEndBlocks;

  /// The value whose ownership we will check.
  SILValue value;

  /// The action that the checker should perform on detecting an error.
  ErrorBehaviorKind errorBehavior;

  /// The list of lifetime ending users that we found. Only valid if check is
  /// successful.
  SmallVector<BranchPropagatedUser, 16> lifetimeEndingUsers;

  /// The list of non lifetime ending users that we found. Only valid if check
  /// is successful.
  SmallVector<BranchPropagatedUser, 16> regularUsers;

  /// The list of implicit non lifetime ending users that we found. This
  /// consists of instructions like end_borrow that end a scoped lifetime. We
  /// must treat those as regular uses and ensure that our value is not
  /// destroyed while that sub-scope is valid.
  ///
  /// TODO: Rename to SubBorrowScopeUsers?
  SmallVector<BranchPropagatedUser, 4> implicitRegularUsers;

  /// The set of blocks that we have visited.
  SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks;

public:
  SILValueOwnershipChecker(
      SILModule &mod, DeadEndBlocks &deadEndBlocks, SILValue value,
      ErrorBehaviorKind errorBehavior,
      llvm::SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks)
      : result(), mod(mod), deadEndBlocks(deadEndBlocks), value(value),
        errorBehavior(errorBehavior), visitedBlocks(visitedBlocks) {
    assert(value && "Can not initialize a checker with an empty SILValue");
  }

  ~SILValueOwnershipChecker() = default;
  SILValueOwnershipChecker(SILValueOwnershipChecker &) = delete;
  SILValueOwnershipChecker(SILValueOwnershipChecker &&) = delete;

  bool check() {
    if (result.hasValue())
      return result.getValue();

    LLVM_DEBUG(llvm::dbgs() << "Verifying ownership of: " << *value);
    result = checkUses();
    if (!result.getValue())
      return false;

    SmallVector<BranchPropagatedUser, 32> allRegularUsers;
    copy(regularUsers, std::back_inserter(allRegularUsers));
    copy(implicitRegularUsers, std::back_inserter(allRegularUsers));
    result =
        valueHasLinearLifetime(value, lifetimeEndingUsers, allRegularUsers,
                               visitedBlocks, deadEndBlocks, errorBehavior);

    return result.getValue();
  }

  using user_array_transform =
      std::function<SILInstruction *(BranchPropagatedUser)>;
  using user_array = TransformArrayRef<user_array_transform>;

  /// A function that returns a range of lifetime ending users found for the
  /// given value.
  user_array getLifetimeEndingUsers() const {
    assert(result.hasValue() && "Can not call until check() is called");
    assert(result.getValue() && "Can not call if check() returned false");

    user_array_transform transform(
        [](BranchPropagatedUser user) -> SILInstruction * {
          return user.getInst();
        });
    return user_array(ArrayRef<BranchPropagatedUser>(lifetimeEndingUsers),
                      transform);
  }

  /// A function that returns a range of regular (i.e. "non lifetime ending")
  /// users found for the given value.
  user_array getRegularUsers() const {
    assert(result.hasValue() && "Can not call until check() is called");
    assert(result.getValue() && "Can not call if check() returned false");

    user_array_transform transform(
        [](BranchPropagatedUser user) -> SILInstruction * {
          return user.getInst();
        });
    return user_array(ArrayRef<BranchPropagatedUser>(regularUsers), transform);
  }

private:
  bool checkUses();
  bool gatherUsers(SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers,
                   SmallVectorImpl<BranchPropagatedUser> &regularUsers,
                   SmallVectorImpl<BranchPropagatedUser> &implicitRegularUsers);

  bool checkValueWithoutLifetimeEndingUses();

  bool checkFunctionArgWithoutLifetimeEndingUses(SILFunctionArgument *arg);
  bool checkYieldWithoutLifetimeEndingUses(BeginApplyResult *yield);

  bool isGuaranteedFunctionArgWithLifetimeEndingUses(
      SILFunctionArgument *arg,
      const SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers) const;
  bool isSubobjectProjectionWithLifetimeEndingUses(
      SILValue value,
      const SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers) const;

  /// Depending on our initialization, either return false or call Func and
  /// throw an error.
  bool handleError(function_ref<void()> &&messagePrinterFunc) const {
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

bool SILValueOwnershipChecker::gatherUsers(
    SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers,
    SmallVectorImpl<BranchPropagatedUser> &nonLifetimeEndingUsers,
    SmallVectorImpl<BranchPropagatedUser> &implicitRegularUsers) {

  // See if Value is guaranteed. If we are guaranteed and not forwarding, then
  // we need to look through subobject uses for more uses. Otherwise, if we are
  // forwarding, we do not create any lifetime ending users/non lifetime ending
  // users since we verify against our base.
  auto ownershipKind = value.getOwnershipKind();
  bool isGuaranteed = ownershipKind == ValueOwnershipKind::Guaranteed;
  bool isOwned = ownershipKind == ValueOwnershipKind::Owned;

  if (isGuaranteed && isGuaranteedForwardingValue(value))
    return true;

  // Then gather up our initial list of users.
  SmallVector<Operand *, 8> users;
  std::copy(value->use_begin(), value->use_end(), std::back_inserter(users));

  auto addCondBranchToList = [](SmallVectorImpl<BranchPropagatedUser> &list,
                                CondBranchInst *cbi, unsigned operandIndex) {
    if (cbi->isConditionOperandIndex(operandIndex)) {
      list.emplace_back(cbi);
      return;
    }

    bool isTrueOperand = cbi->isTrueOperandIndex(operandIndex);
    list.emplace_back(cbi, isTrueOperand ? CondBranchInst::TrueIdx
                                         : CondBranchInst::FalseIdx);
  };

  bool foundError = false;
  while (!users.empty()) {
    Operand *op = users.pop_back_val();
    SILInstruction *user = op->getUser();

    // If this op is a type dependent operand, skip it. It is not interesting
    // from an ownership perspective.
    if (user->isTypeDependentOperand(*op))
      continue;

    bool isGuaranteedSubValue = false;
    if (isGuaranteed && isGuaranteedForwardingInst(op->getUser())) {
      isGuaranteedSubValue = true;
    }

    auto opOwnershipKindMap = op->getOwnershipKindMap(isGuaranteedSubValue);
    // If our ownership kind doesn't match, track that we found an error, emit
    // an error message optionally and then continue.
    if (!opOwnershipKindMap.canAcceptKind(ownershipKind)) {
      foundError = true;

      // If we did not support /any/ ownership kind, it means that we found a
      // conflicting answer so the kind map that was returned is the empty
      // map. Put out a more specific error here.
      if (!opOwnershipKindMap.data.any()) {
        handleError([&]() {
          llvm::errs() << "Function: '" << user->getFunction()->getName()
                       << "'\n"
                       << "Ill-formed SIL! Unable to compute ownership kind "
                          "map for user?!\n"
                       << "For terminator users, check that successors have "
                          "compatible ownership kinds.\n"
                       << "Value: " << op->get() << "User: " << *user
                       << "Operand Number: " << op->getOperandNumber() << '\n'
                       << "Conv: " << ownershipKind << "\n\n";
        });
        continue;
      }

      handleError([&]() {
        llvm::errs() << "Function: '" << user->getFunction()->getName() << "'\n"
                     << "Have operand with incompatible ownership?!\n"
                     << "Value: " << op->get() << "User: " << *user
                     << "Operand Number: " << op->getOperandNumber() << '\n'
                     << "Conv: " << ownershipKind << '\n'
                     << "OwnershipMap:\n"
                     << opOwnershipKindMap << '\n';
      });
      continue;
    }

    auto lifetimeConstraint =
        opOwnershipKindMap.getLifetimeConstraint(ownershipKind);
    if (lifetimeConstraint == UseLifetimeConstraint::MustBeInvalidated) {
      LLVM_DEBUG(llvm::dbgs() << "        Lifetime Ending User: " << *user);
      if (auto *cbi = dyn_cast<CondBranchInst>(user)) {
        addCondBranchToList(lifetimeEndingUsers, cbi, op->getOperandNumber());
      } else {
        lifetimeEndingUsers.emplace_back(user);
      }
    } else {
      LLVM_DEBUG(llvm::dbgs() << "        Regular User: " << *user);
      if (auto *cbi = dyn_cast<CondBranchInst>(user)) {
        addCondBranchToList(nonLifetimeEndingUsers, cbi,
                            op->getOperandNumber());
      } else {
        nonLifetimeEndingUsers.emplace_back(user);
      }
    }

    // If our base value is not guaranteed, we do not to try to visit
    // subobjects.
    if (!isGuaranteed) {
      // But if we are owned, check if we have any end_borrows. We
      // need to treat these as sub-scope users. We can rely on the
      // end_borrow to prevent recursion.
      if (isOwned) {
        // Do a check if any of our users are begin_borrows. If we find such a
        // use, then we want to include the end_borrow associated with the
        // begin_borrow in our NonLifetimeEndingUser lists.
        //
        // For correctness reasons we use indices to make sure that we can
        // append to NonLifetimeEndingUsers without needing to deal with
        // iterator invalidation.
        SmallVector<SILInstruction *, 4> endBorrowInsts;
        for (unsigned i : indices(nonLifetimeEndingUsers)) {
          if (auto *bbi = dyn_cast<BeginBorrowInst>(
                  nonLifetimeEndingUsers[i].getInst())) {
            copy(bbi->getEndBorrows(),
                 std::back_inserter(implicitRegularUsers));
          }
        }
      }
      continue;
    }

    // If we are guaranteed, but are not a guaranteed forwarding inst,
    // just continue. This user is just treated as a normal use.
    if (!isGuaranteedForwardingInst(user))
      continue;

    // At this point, we know that we must have a forwarded subobject. Since the
    // base type is guaranteed, we know that the subobject is either guaranteed
    // or trivial. We now split into two cases, if the user is a terminator or
    // not. If we do not have a terminator, then just add the uses of all of
    // User's results to the worklist.
    if (user->getResults().size()) {
      for (SILValue result : user->getResults()) {
        if (result.getOwnershipKind() == ValueOwnershipKind::Trivial) {
          continue;
        }

        // Now, we /must/ have a guaranteed subobject, so let's assert that the
        // user is actually guaranteed and add the subobject's users to our
        // worklist.
        assert(result.getOwnershipKind() == ValueOwnershipKind::Guaranteed &&
               "Our value is guaranteed and this is a forwarding instruction. "
               "Should have guaranteed ownership as well.");
        copy(result->getUses(), std::back_inserter(users));
      }

      continue;
    }

    assert(user->getResults().empty());

    auto *ti = dyn_cast<TermInst>(user);
    if (!ti) {
      continue;
    }

    // Otherwise if we have a terminator, add any as uses any end_borrow to
    // ensure that the subscope is completely enclsed within the super scope. We
    // require all of our arguments to be either trivial or guaranteed.
    for (auto &succ : ti->getSuccessors()) {
      auto *succBlock = succ.getBB();

      // If we do not have any arguments, then continue.
      if (succBlock->args_empty())
        continue;

      // Otherwise, make sure that all arguments are trivial or guaranteed. If
      // we fail, emit an error.
      //
      // TODO: We could ignore this error and emit a more specific error on the
      // actual terminator.
      for (auto *succArg : succBlock->getPhiArguments()) {
        // *NOTE* We do not emit an error here since we want to allow for more
        // specific errors to be found during use_verification.
        //
        // TODO: Add a flag that associates the terminator instruction with
        // needing to be verified. If it isn't verified appropriately, assert
        // when the verifier is destroyed.
        auto succArgOwnershipKind = succArg->getOwnershipKind();
        if (!succArgOwnershipKind.isTrivialOrCompatibleWith(ownershipKind)) {
          // This is where the error would go.
          continue;
        }

        // If we have a trivial value, just continue.
        if (succArgOwnershipKind == ValueOwnershipKind::Trivial)
          continue;

        // Otherwise add all end_borrow users for this BBArg to the
        // implicit regular user list. We know that BBArg must be
        // completely joint post-dominated by these users, so we use
        // them to ensure that all of BBArg's uses are completely
        // enclosed within the end_borrow of this argument.
        for (auto *op : succArg->getUses()) {
          if (auto *ebi = dyn_cast<EndBorrowInst>(op->getUser())) {
            implicitRegularUsers.push_back(ebi);
          }
        }
      }
    }
  }

  // Return true if we did not have an error and false if we did find an error.
  //
  // The reason why we use this extra variable is to make sure that when we are
  // testing, we print out all mismatching pairs rather than just the first.
  return !foundError;
}

bool SILValueOwnershipChecker::checkFunctionArgWithoutLifetimeEndingUses(
    SILFunctionArgument *arg) {
  switch (arg->getOwnershipKind()) {
  case ValueOwnershipKind::Guaranteed:
  case ValueOwnershipKind::Unowned:
  case ValueOwnershipKind::Trivial:
    return true;
  case ValueOwnershipKind::Any:
    llvm_unreachable(
        "Function arguments should never have ValueOwnershipKind::Any");
  case ValueOwnershipKind::Owned:
    break;
  }

  if (deadEndBlocks.isDeadEnd(arg->getParent()))
    return true;

  return !handleError([&] {
    llvm::errs() << "Function: '" << arg->getFunction()->getName() << "'\n"
                 << "    Owned function parameter without life ending uses!\n"
                 << "Value: " << *arg << '\n';
  });
}

bool SILValueOwnershipChecker::checkYieldWithoutLifetimeEndingUses(
    BeginApplyResult *yield) {
  switch (yield->getOwnershipKind()) {
  case ValueOwnershipKind::Guaranteed:
  case ValueOwnershipKind::Unowned:
  case ValueOwnershipKind::Trivial:
    return true;
  case ValueOwnershipKind::Any:
    llvm_unreachable("Yields should never have ValueOwnershipKind::Any");
  case ValueOwnershipKind::Owned:
    break;
  }

  if (deadEndBlocks.isDeadEnd(yield->getParent()->getParent()))
    return true;

  return !handleError([&] {
    llvm::errs() << "Function: '" << yield->getFunction()->getName() << "'\n"
                 << "    Owned yield without life ending uses!\n"
                 << "Value: " << *yield << '\n';
  });
}
bool SILValueOwnershipChecker::checkValueWithoutLifetimeEndingUses() {
  LLVM_DEBUG(llvm::dbgs() << "    No lifetime ending users?! Bailing early.\n");
  if (auto *arg = dyn_cast<SILFunctionArgument>(value)) {
    if (checkFunctionArgWithoutLifetimeEndingUses(arg)) {
      return true;
    }
  }

  if (auto *yield = dyn_cast<BeginApplyResult>(value)) {
    if (checkYieldWithoutLifetimeEndingUses(yield)) {
      return true;
    }
  }

  // Check if we are a guaranteed subobject. In such a case, we should never
  // have lifetime ending uses, since our lifetime is guaranteed by our
  // operand, so there is nothing further to do. So just return true.
  if (isGuaranteedForwardingValue(value) &&
      value.getOwnershipKind() == ValueOwnershipKind::Guaranteed)
    return true;

  // If we have an unowned value, then again there is nothing left to do.
  if (value.getOwnershipKind() == ValueOwnershipKind::Unowned)
    return true;

  if (auto *parentBlock = value->getParentBlock()) {
    if (deadEndBlocks.isDeadEnd(parentBlock)) {
      LLVM_DEBUG(llvm::dbgs() << "    Ignoring transitively unreachable value "
                              << "without users!\n"
                              << "    Function: '"
                              << value->getFunction()->getName() << "'\n"
                              << "    Value: " << *value << '\n');
      return true;
    }
  }

  if (!isValueAddressOrTrivial(value, mod)) {
    return !handleError([&] {
      llvm::errs() << "Function: '" << value->getFunction()->getName() << "'\n";
      if (value.getOwnershipKind() == ValueOwnershipKind::Owned) {
        llvm::errs() << "Error! Found a leaked owned value that was never "
                        "consumed.\n";
      } else {
        llvm::errs() << "Non trivial values, non address values, and non "
                        "guaranteed function args must have at least one "
                        "lifetime ending use?!\n";
      }
      llvm::errs() << "Value: " << *value << '\n';
    });
  }

  return true;
}

bool SILValueOwnershipChecker::isGuaranteedFunctionArgWithLifetimeEndingUses(
    SILFunctionArgument *arg,
    const llvm::SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers)
    const {
  if (arg->getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return true;

  return handleError([&] {
    llvm::errs() << "    Function: '" << arg->getFunction()->getName() << "'\n"
                 << "    Guaranteed function parameter with life ending uses!\n"
                 << "    Value: " << *arg;
    for (const auto &user : lifetimeEndingUsers) {
      llvm::errs() << "    Lifetime Ending User: " << *user;
    }
    llvm::errs() << '\n';
  });
}

bool SILValueOwnershipChecker::isSubobjectProjectionWithLifetimeEndingUses(
    SILValue value,
    const llvm::SmallVectorImpl<BranchPropagatedUser> &lifetimeEndingUsers)
    const {
  return handleError([&] {
    llvm::errs() << "    Function: '" << value->getFunction()->getName()
                 << "'\n"
                 << "    Subobject projection with life ending uses!\n"
                 << "    Value: " << *value;
    for (const auto &user : lifetimeEndingUsers) {
      llvm::errs() << "    Lifetime Ending User: " << *user;
    }
    llvm::errs() << '\n';
  });
}

bool SILValueOwnershipChecker::checkUses() {
  LLVM_DEBUG(llvm::dbgs() << "    Gathering and classifying uses!\n");

  // First go through V and gather up its uses. While we do this we:
  //
  // 1. Verify that none of the uses are in the same block. This would be an
  // overconsume so in this case we assert.
  // 2. Verify that the uses are compatible with our ownership convention.
  if (!gatherUsers(lifetimeEndingUsers, regularUsers, implicitRegularUsers)) {
    // Silently return false if this fails.
    //
    // If the user pass in a ErrorBehaviorKind that will assert, we
    // will have asserted in gatherUsers(). If we get here the user
    // asked us to optionally print out a message and indicate that
    // the verification failed.
    return false;
  }

  // We can only have no lifetime ending uses if we have:
  //
  // 1. A trivial typed value.
  // 2. An address type value.
  // 3. A guaranteed function argument.
  //
  // In the first two cases, it is easy to see that there is nothing further to
  // do but return false.
  //
  // In the case of a function argument, one must think about the issues a bit
  // more. Specifically, we should have /no/ lifetime ending uses of a
  // guaranteed function argument, since a guaranteed function argument should
  // outlive the current function always.
  if (lifetimeEndingUsers.empty() && checkValueWithoutLifetimeEndingUses()) {
    return false;
  }

  LLVM_DEBUG(llvm::dbgs() << "    Found lifetime ending users! Performing "
                             "initial checks\n");

  // See if we have a guaranteed function address. Guaranteed function addresses
  // should never have any lifetime ending uses.
  if (auto *arg = dyn_cast<SILFunctionArgument>(value)) {
    if (!isGuaranteedFunctionArgWithLifetimeEndingUses(arg,
                                                       lifetimeEndingUsers)) {
      return false;
    }
  }

  // Check if we are an instruction that forwards forwards guaranteed
  // ownership. In such a case, we are a subobject projection. We should not
  // have any lifetime ending uses.
  if (isGuaranteedForwardingValue(value) &&
      value.getOwnershipKind() == ValueOwnershipKind::Guaranteed) {
    if (!isSubobjectProjectionWithLifetimeEndingUses(value,
                                                     lifetimeEndingUsers)) {
      return false;
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entrypoints
//===----------------------------------------------------------------------===//

void SILInstruction::verifyOperandOwnership() const {
#ifndef NDEBUG
  if (DisableOwnershipVerification)
    return;

  if (isStaticInitializerInst())
    return;

  // If SILOwnership is not enabled, do not perform verification.
  if (!getModule().getOptions().EnableSILOwnership)
    return;

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!getFunction()->hasQualifiedOwnership() ||
      !getFunction()->shouldVerifyOwnership())
    return;

  // If we are testing the verifier, bail so we only print errors once when
  // performing a full verification, instead of additionally in the SILBuilder.
  if (IsSILOwnershipVerifierTestingEnabled)
    return;

  // If this is a terminator instruction, do not verify in SILBuilder. This is
  // because when building a new function, one must create the destination block
  // first which is an unnatural pattern and pretty brittle.
  if (isa<TermInst>(this))
    return;

  ErrorBehaviorKind errorBehavior;
  if (IsSILOwnershipVerifierTestingEnabled) {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndReturnFalse;
  } else {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndAssert;
  }
  for (const Operand &op : getAllOperands()) {
    // Skip type dependence operands.
    if (isTypeDependentOperand(op))
      continue;
    SILValue opValue = op.get();

    auto operandOwnershipKindMap = op.getOwnershipKindMap();
    auto valueOwnershipKind = opValue.getOwnershipKind();
    if (operandOwnershipKindMap.canAcceptKind(valueOwnershipKind))
      continue;

    if (errorBehavior.shouldPrintMessage()) {
      llvm::errs() << "Found an operand with a value that is not compatible "
                      "with the operand's operand ownership kind map.\n";
      llvm::errs() << "Value: " << opValue;
      llvm::errs() << "Value Ownership Kind: " << valueOwnershipKind << "\n";
      llvm::errs() << "Instruction: " << *this;
      llvm::errs() << "Operand Ownership Kind Map: " << operandOwnershipKindMap;
    }

    if (errorBehavior.shouldReturnFalse())
      continue;

    assert(errorBehavior.shouldAssert() &&
           "At this point, we are expected to assert");
    llvm_unreachable("triggering standard assertion failure routine");
  }
#endif
}

void SILValue::verifyOwnership(SILModule &mod,
                               DeadEndBlocks *deadEndBlocks) const {
#ifndef NDEBUG
  if (DisableOwnershipVerification)
    return;

  // Since we do not have SILUndef, we now know that getFunction() should return
  // a real function. Assert in case this assumption is no longer true.
  SILFunction *f = (*this)->getFunction();
  assert(f && "Instructions and arguments should have a function");

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!f->hasQualifiedOwnership() || !f->shouldVerifyOwnership())
    return;

  assert(getOwnershipKind() != ValueOwnershipKind::Any &&
         "No values should have any ownership anymore");
  ErrorBehaviorKind errorBehavior;
  if (IsSILOwnershipVerifierTestingEnabled) {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndReturnFalse;
  } else {
    errorBehavior = ErrorBehaviorKind::PrintMessageAndAssert;
  }

  llvm::SmallPtrSet<SILBasicBlock *, 32> liveBlocks;
  if (deadEndBlocks) {
    SILValueOwnershipChecker(mod, *deadEndBlocks, *this, errorBehavior,
                             liveBlocks)
        .check();
  } else {
    DeadEndBlocks deadEndBlocks(f);
    SILValueOwnershipChecker(mod, deadEndBlocks, *this, errorBehavior,
                             liveBlocks)
        .check();
  }
#endif
}

bool OwnershipChecker::checkValue(SILValue value) {
  regularUsers.clear();
  lifetimeEndingUsers.clear();
  liveBlocks.clear();

  // Since we do not have SILUndef, we now know that getFunction() should return
  // a real function. Assert in case this assumption is no longer true.
  SILFunction *f = value->getFunction();
  assert(f && "Instructions and arguments should have a function");

  // If the given function has unqualified ownership, there is nothing further
  // to verify.
  if (!f->hasQualifiedOwnership())
    return false;

  ErrorBehaviorKind errorBehavior(ErrorBehaviorKind::ReturnFalse);
  SILValueOwnershipChecker checker(mod, deadEndBlocks, value, errorBehavior,
                                   liveBlocks);
  if (!checker.check()) {
    return false;
  }

  // TODO: Make this more efficient.
  copy(checker.getRegularUsers(), std::back_inserter(regularUsers));
  copy(checker.getLifetimeEndingUsers(),
       std::back_inserter(lifetimeEndingUsers));
  return true;
}
