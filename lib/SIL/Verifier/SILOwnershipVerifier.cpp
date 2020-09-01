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

#include "LinearLifetimeCheckerPrivate.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/InstructionUtils.h"
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

namespace swift {

// TODO: This class uses a bunch of global state like variables. It should be
// refactored into a large state object that is used by functions.
class SILValueOwnershipChecker {
  /// The result of performing the check.
  Optional<bool> result;

  /// A cache of dead-end basic blocks that we use to determine if we can
  /// ignore "leaks".
  DeadEndBlocks &deadEndBlocks;

  /// The value whose ownership we will check.
  SILValue value;

  /// The builder that the checker uses to emit error messages, crash if asked
  /// for, or supply back interesting info to the caller.
  LinearLifetimeChecker::ErrorBuilder &errorBuilder;

  /// The list of lifetime ending users that we found. Only valid if check is
  /// successful.
  SmallVector<Operand *, 16> lifetimeEndingUsers;

  /// The list of non lifetime ending users that we found. Only valid if check
  /// is successful.
  SmallVector<Operand *, 16> regularUsers;

  /// The set of blocks that we have visited.
  SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks;

public:
  SILValueOwnershipChecker(
      DeadEndBlocks &deadEndBlocks, SILValue value,
      LinearLifetimeChecker::ErrorBuilder &errorBuilder,
      llvm::SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks)
      : result(), deadEndBlocks(deadEndBlocks), value(value),
        errorBuilder(errorBuilder), visitedBlocks(visitedBlocks) {
    assert(value && "Can not initialize a checker with an empty SILValue");
  }

  ~SILValueOwnershipChecker() = default;
  SILValueOwnershipChecker(SILValueOwnershipChecker &) = delete;
  SILValueOwnershipChecker(SILValueOwnershipChecker &&) = delete;

  bool check();

  StringRef getFunctionName() const { return value->getFunction()->getName(); }

private:
  bool checkUses();
  bool isCompatibleDefUse(Operand *op, ValueOwnershipKind ownershipKind);

  bool gatherUsers(SmallVectorImpl<Operand *> &lifetimeEndingUsers,
                   SmallVectorImpl<Operand *> &regularUsers);

  bool gatherNonGuaranteedUsers(SmallVectorImpl<Operand *> &lifetimeEndingUsers,
                                SmallVectorImpl<Operand *> &regularUsers);

  bool checkValueWithoutLifetimeEndingUses();

  bool checkFunctionArgWithoutLifetimeEndingUses(SILFunctionArgument *arg);
  bool checkYieldWithoutLifetimeEndingUses(BeginApplyResult *yield);

  bool isGuaranteedFunctionArgWithLifetimeEndingUses(
      SILFunctionArgument *arg,
      const SmallVectorImpl<Operand *> &lifetimeEndingUsers) const;
  bool isSubobjectProjectionWithLifetimeEndingUses(
      SILValue value,
      const SmallVectorImpl<Operand *> &lifetimeEndingUsers) const;
};

} // namespace swift

bool SILValueOwnershipChecker::check() {
  if (result.hasValue())
    return result.getValue();

  LLVM_DEBUG(llvm::dbgs() << "Verifying ownership of: " << *value);
  result = checkUses();
  if (!result.getValue())
    return false;

  SmallVector<Operand *, 32> allLifetimeEndingUsers;
  llvm::copy(lifetimeEndingUsers, std::back_inserter(allLifetimeEndingUsers));
  SmallVector<Operand *, 32> allRegularUsers;
  llvm::copy(regularUsers, std::back_inserter(allRegularUsers));

  LinearLifetimeChecker checker(visitedBlocks, deadEndBlocks);
  auto linearLifetimeResult = checker.checkValue(value, allLifetimeEndingUsers,
                                                 allRegularUsers, errorBuilder);
  result = !linearLifetimeResult.getFoundError();

  return result.getValue();
}

bool SILValueOwnershipChecker::isCompatibleDefUse(
    Operand *op, ValueOwnershipKind ownershipKind) {
  bool isGuaranteedSubValue = false;
  if (ownershipKind == ValueOwnershipKind::Guaranteed &&
      isGuaranteedForwardingInst(op->getUser())) {
    isGuaranteedSubValue = true;
  }
  auto *user = op->getUser();
  auto opOwnershipKindMap = op->getOwnershipKindMap(isGuaranteedSubValue);
  // If our ownership kind doesn't match, track that we found an error, emit
  // an error message optionally and then continue.
  if (opOwnershipKindMap.canAcceptKind(ownershipKind)) {
    return true;
  }

  // If we did not support /any/ ownership kind, it means that we found a
  // conflicting answer so the kind map that was returned is the empty
  // map. Put out a more specific error here.
  if (!opOwnershipKindMap.data.any()) {
    errorBuilder.handleMalformedSIL([&]() {
      llvm::errs() << "Ill-formed SIL! Unable to compute ownership kind "
                      "map for user?!\n"
                   << "For terminator users, check that successors have "
                      "compatible ownership kinds.\n"
                   << "Value: " << op->get() << "User: " << *user
                   << "Operand Number: " << op->getOperandNumber() << '\n'
                   << "Conv: " << ownershipKind << "\n\n";
    });
    return false;
  }

  errorBuilder.handleMalformedSIL([&]() {
    llvm::errs() << "Have operand with incompatible ownership?!\n"
                 << "Value: " << op->get() << "User: " << *user
                 << "Operand Number: " << op->getOperandNumber() << '\n'
                 << "Conv: " << ownershipKind << '\n'
                 << "OwnershipMap:\n"
                 << opOwnershipKindMap << '\n';
  });
  return false;
}

bool SILValueOwnershipChecker::gatherNonGuaranteedUsers(
    SmallVectorImpl<Operand *> &lifetimeEndingUsers,
    SmallVectorImpl<Operand *> &nonLifetimeEndingUsers) {
  bool foundError = false;

  auto ownershipKind = value.getOwnershipKind();
  bool isOwned = ownershipKind == ValueOwnershipKind::Owned;

  // Since we are dealing with a non-guaranteed user, we do not have to recurse.
  for (auto *op : value->getUses()) {
    auto *user = op->getUser();

    // If this op is a type dependent operand, skip it. It is not interesting
    // from an ownership perspective.
    if (user->isTypeDependentOperand(*op))
      continue;

    // First check if this recursive use is compatible with our values ownership
    // kind. If not, flag the error and continue so that we can report more
    // errors.
    if (!isCompatibleDefUse(op, ownershipKind)) {
      foundError = true;
      continue;
    }

    // First do a quick check if we have a consuming use. If so, stash the value
    // and continue.
    if (op->isConsumingUse()) {
      LLVM_DEBUG(llvm::dbgs() << "Lifetime Ending User: " << *user);
      lifetimeEndingUsers.push_back(op);
      continue;
    }

    // Otherwise, we have a non lifetime ending user. Add it to our non lifetime
    // ending user list.
    LLVM_DEBUG(llvm::dbgs() << "Regular User: " << *user);
    nonLifetimeEndingUsers.push_back(op);

    // If we do not have an owned value at this point, continue, we do not have
    // any further work to do.
    if (!isOwned) {
      continue;
    }

    // Otherwise, check if we have a borrow scope operand. In such a case, we
    // need to add the borrow scope operand's end scope instructions as implicit
    // regular users so we can ensure that the borrow scope operand's scope is
    // completely within the owned value's scope. If we do not have a borrow
    // scope operand, just continue, we are done.
    auto initialScopedOperand = BorrowingOperand::get(op);
    if (!initialScopedOperand) {
      continue;
    }

    // If our scoped operand is not also a borrow introducer, then we know that
    // we do not need to consider guaranteed phis and thus can just add the
    // initial end scope instructions without any further work.
    //
    // Maybe: Is borrow scope non-local?
    std::function<void(Operand *)> error = [&](Operand *op) {
      errorBuilder.handleMalformedSIL([&] {
        llvm::errs() << "Implicit Regular User Guaranteed Phi Cycle!\n"
                     << "User: " << *op->getUser()
                     << "Initial: " << *initialScopedOperand << "\n";
      });
    };
    foundError |=
        initialScopedOperand->getImplicitUses(nonLifetimeEndingUsers, &error);
  }

  return foundError;
}

bool SILValueOwnershipChecker::gatherUsers(
    SmallVectorImpl<Operand *> &lifetimeEndingUsers,
    SmallVectorImpl<Operand *> &nonLifetimeEndingUsers) {

  // See if Value is guaranteed. If we are guaranteed and not forwarding, then
  // we need to look through subobject uses for more uses. Otherwise, if we are
  // forwarding, we do not create any lifetime ending users/non lifetime ending
  // users since we verify against our base.
  if (value.getOwnershipKind() != ValueOwnershipKind::Guaranteed) {
    return !gatherNonGuaranteedUsers(lifetimeEndingUsers,
                                     nonLifetimeEndingUsers);
  }

  // Ok, we have a value with guarantee ownership. Before we continue, check if
  // this value forwards guaranteed ownership. In such a case, we are going to
  // validate it as part of the borrow introducer from which the forwarding
  // value originates. So we can just return true and continue.
  if (isGuaranteedForwardingValue(value))
    return true;

  // Ok, we have some sort of borrow introducer. We need to recursively validate
  // that all of its uses (including sub-scopes) are before any end_borrows that
  // end the lifetime of the borrow introducer. With that in mind, gather up our
  // initial list of users.
  SmallVector<Operand *, 8> users;
  llvm::copy(value->getUses(), std::back_inserter(users));

  bool foundError = false;
  while (!users.empty()) {
    Operand *op = users.pop_back_val();
    SILInstruction *user = op->getUser();

    // If this op is a type dependent operand, skip it. It is not interesting
    // from an ownership perspective.
    if (user->isTypeDependentOperand(*op))
      continue;

    // First check if this recursive use is compatible with our values
    // ownership kind. If not, flag the error and continue so that we can
    // report more errors.
    if (!isCompatibleDefUse(op, ValueOwnershipKind::Guaranteed)) {
      foundError = true;
      continue;
    }

    // If we are visiting a non-first level user and we
    // If we are guaranteed, but are not a guaranteed forwarding inst, we add
    // the end scope instructions of any new sub-scopes. This ensures that the
    // parent scope completely encloses the child borrow scope.
    //
    // Example: A guaranteed parameter of a co-routine.

    // Now check if we have a non guaranteed forwarding inst...
    if (!isGuaranteedForwardingInst(user)) {
      // First check if we are visiting an operand that is a consuming use...
      if (op->isConsumingUse()) {
        // If its underlying value is our original value, then this is a true
        // lifetime ending use. Otherwise, we have a guaranteed value that has
        // an end_borrow on a forwarded value which is not supported in any
        // case, so emit an error.
        if (op->get() != value) {
          errorBuilder.handleMalformedSIL([&] {
            llvm::errs() << "Invalid End Borrow!\n"
                         << "Original Value: " << value
                         << "End Borrow: " << *op->getUser() << "\n";
          });
          foundError = true;
          continue;
        }

        // Otherwise, track this as a lifetime ending use of our underlying
        // value and continue.
        LLVM_DEBUG(llvm::dbgs() << "Lifetime Ending User: " << *user);
        lifetimeEndingUsers.push_back(op);
        continue;
      }

      // Ok, our operand does not consume guaranteed values. Check if it is a
      // BorrowScopeOperand and if so, add its end scope instructions as
      // implicit regular users of our value.
      if (auto scopedOperand = BorrowingOperand::get(op)) {
        assert(!scopedOperand->consumesGuaranteedValues());

        std::function<void(Operand *)> onError = [&](Operand *op) {
          errorBuilder.handleMalformedSIL([&] {
            llvm::errs() << "Implicit Regular User Guaranteed Phi Cycle!\n"
                         << "User: " << *op->getUser()
                         << "Initial: " << *scopedOperand << "\n";
          });
        };
        foundError |=
            scopedOperand->getImplicitUses(nonLifetimeEndingUsers, &onError);
      }

      // Next see if our use is an interior pointer operand. If we have an
      // interior pointer, we need to add all of its address uses as "implicit
      // regular users" of our consumed value.
      if (auto interiorPointerOperand = InteriorPointerOperand::get(op)) {
        std::function<void(Operand *)> onError = [&](Operand *op) {
          errorBuilder.handleMalformedSIL([&] {
            llvm::errs() << "Could not recognize address user of interior "
                            "pointer operand!\n"
                         << "Interior Pointer Operand: "
                         << interiorPointerOperand->operand->getUser()
                         << "Address User: " << *op->getUser();
          });
        };
        foundError |= interiorPointerOperand->getImplicitUses(
            nonLifetimeEndingUsers, &onError);
      }

      // Finally add the op to the non lifetime ending user list.
      LLVM_DEBUG(llvm::dbgs() << "Regular User: " << *user);
      nonLifetimeEndingUsers.push_back(op);
      continue;
    }

    // At this point since we have a forwarded subobject, we know this is a non
    // lifetime ending user.
    LLVM_DEBUG(llvm::dbgs() << "Regular User: " << *user);
    nonLifetimeEndingUsers.push_back(op);

    // At this point, we know that we must have a forwarded subobject. Since
    // the base type is guaranteed, we know that the subobject is either
    // guaranteed or trivial. We now split into two cases, if the user is a
    // terminator or not. If we do not have a terminator, then just add the
    // uses of all of User's results to the worklist.
    if (user->getResults().size()) {
      for (SILValue result : user->getResults()) {
        if (result.getOwnershipKind() == ValueOwnershipKind::None) {
          continue;
        }

        // Now, we /must/ have a guaranteed subobject, so let's assert that
        // the user is actually guaranteed and add the subobject's users to
        // our worklist.
        assert(result.getOwnershipKind() == ValueOwnershipKind::Guaranteed &&
               "Our value is guaranteed and this is a forwarding instruction. "
               "Should have guaranteed ownership as well.");
        llvm::copy(result->getUses(), std::back_inserter(users));
      }

      continue;
    }

    assert(user->getResults().empty());
    auto *ti = dyn_cast<TermInst>(user);
    if (!ti) {
      continue;
    }

    // At this point, the only type of thing we could have is a transformation
    // terminator since all forwarding terminators are transformation
    // terminators.
    assert(ti->isTransformationTerminator() &&
           "Out of sync with isTransformationTerminator()");
    for (auto *succBlock : ti->getSuccessorBlocks()) {
      // If we do not have any arguments, then continue.
      if (succBlock->args_empty())
        continue;

      // Otherwise, make sure that all arguments are trivial or guaranteed.
      // If we fail, emit an error.
      //
      // TODO: We could ignore this error and emit a more specific error on
      // the actual terminator.
      for (auto *succArg : succBlock->getSILPhiArguments()) {
        // *NOTE* We do not emit an error here since we want to allow for
        // more specific errors to be found during use_verification.
        //
        // TODO: Add a flag that associates the terminator instruction with
        // needing to be verified. If it isn't verified appropriately,
        // assert when the verifier is destroyed.
        auto succArgOwnershipKind = succArg->getOwnershipKind();
        if (!succArgOwnershipKind.isCompatibleWith(
                ValueOwnershipKind::Guaranteed)) {
          // This is where the error would go.
          continue;
        }

        // If we have an any value, just continue.
        if (succArgOwnershipKind == ValueOwnershipKind::None)
          continue;

        // Otherwise add all users of this BBArg to the worklist to visit
        // recursively.
        llvm::copy(succArg->getUses(), std::back_inserter(users));
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
  case ValueOwnershipKind::None:
    return true;
  case ValueOwnershipKind::Owned:
    break;
  }

  if (deadEndBlocks.isDeadEnd(arg->getParent()))
    return true;

  return !errorBuilder.handleMalformedSIL([&] {
    llvm::errs() << "Owned function parameter without life ending uses!\n"
                 << "Value: " << *arg << '\n';
  });
}

bool SILValueOwnershipChecker::checkYieldWithoutLifetimeEndingUses(
    BeginApplyResult *yield) {
  switch (yield->getOwnershipKind()) {
  case ValueOwnershipKind::Guaranteed:
  case ValueOwnershipKind::Unowned:
  case ValueOwnershipKind::None:
    return true;
  case ValueOwnershipKind::Owned:
    break;
  }

  if (deadEndBlocks.isDeadEnd(yield->getParent()->getParent()))
    return true;

  return !errorBuilder.handleMalformedSIL([&] {
    llvm::errs() << "Owned yield without life ending uses!\n"
                 << "Value: " << *yield << '\n';
  });
}
bool SILValueOwnershipChecker::checkValueWithoutLifetimeEndingUses() {
  LLVM_DEBUG(llvm::dbgs() << "No lifetime ending users?! Bailing early.\n");
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
      LLVM_DEBUG(llvm::dbgs() << "Ignoring transitively unreachable value "
                              << "without users!\n"
                              << "    Value: " << *value << '\n');
      return true;
    }
  }

  if (!isValueAddressOrTrivial(value)) {
    return !errorBuilder.handleMalformedSIL([&] {
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
    const llvm::SmallVectorImpl<Operand *> &lifetimeEndingUsers) const {
  if (arg->getOwnershipKind() != ValueOwnershipKind::Guaranteed)
    return true;

  return errorBuilder.handleMalformedSIL([&] {
    llvm::errs() << "Guaranteed function parameter with life ending uses!\n"
                 << "Value: " << *arg;
    for (const auto *use : lifetimeEndingUsers) {
      llvm::errs() << "Lifetime Ending User: " << *use->getUser();
    }
    llvm::errs() << '\n';
  });
}

bool SILValueOwnershipChecker::isSubobjectProjectionWithLifetimeEndingUses(
    SILValue value,
    const llvm::SmallVectorImpl<Operand *> &lifetimeEndingUsers) const {
  return errorBuilder.handleMalformedSIL([&] {
    llvm::errs() << "Subobject projection with life ending uses!\n"
                 << "Value: " << *value;
    for (const auto *use : lifetimeEndingUsers) {
      llvm::errs() << "Lifetime Ending User: " << *use->getUser();
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
  if (!gatherUsers(lifetimeEndingUsers, regularUsers)) {
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
  if (DisableOwnershipVerification)
    return;

  if (isStaticInitializerInst())
    return;

#ifdef NDEBUG
  // When compiling without asserts enabled, only verify ownership if
  // -sil-verify-all is set.
  if (!getModule().getOptions().VerifyAll)
    return;
#endif

  // If SILOwnership is not enabled, do not perform verification.
  if (!getModule().getOptions().VerifySILOwnership)
    return;

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!getFunction()->hasOwnership() ||
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

  using BehaviorKind = LinearLifetimeChecker::ErrorBehaviorKind;
  Optional<LinearLifetimeChecker::ErrorBuilder> errorBuilder;
  if (IsSILOwnershipVerifierTestingEnabled) {
    errorBuilder.emplace(*getFunction(),
                         BehaviorKind::PrintMessageAndReturnFalse);
  } else {
    errorBuilder.emplace(*getFunction(), BehaviorKind::PrintMessageAndAssert);
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

    errorBuilder->handleMalformedSIL([&] {
      llvm::errs() << "Found an operand with a value that is not compatible "
                      "with the operand's operand ownership kind map.\n";
      llvm::errs() << "Value: " << opValue;
      llvm::errs() << "Value Ownership Kind: " << valueOwnershipKind << "\n";
      llvm::errs() << "Instruction:\n";
      printInContext(llvm::errs());
      llvm::errs() << "Operand Ownership Kind Map: " << operandOwnershipKindMap;
    });
  }
}

static void
verifySILValueHelper(const SILFunction *f, SILValue value,
                     LinearLifetimeChecker::ErrorBuilder &errorBuilder,
                     DeadEndBlocks *deadEndBlocks) {
  assert(!isa<SILUndef>(value) &&
         "We assume we are always passed arguments or instruction results");

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!f->hasOwnership() || !f->shouldVerifyOwnership())
    return;

  SmallPtrSet<SILBasicBlock *, 32> liveBlocks;
  if (deadEndBlocks) {
    SILValueOwnershipChecker(*deadEndBlocks, value, errorBuilder, liveBlocks)
        .check();
  } else {
    DeadEndBlocks deadEndBlocks(f);
    SILValueOwnershipChecker(deadEndBlocks, value, errorBuilder, liveBlocks)
        .check();
  }
}

void SILValue::verifyOwnership(DeadEndBlocks *deadEndBlocks) const {
  if (DisableOwnershipVerification)
    return;

  // Do not validate SILUndef values.
  if (isa<SILUndef>(*this))
    return;

#ifdef NDEBUG
  // When compiling without asserts enabled, only verify ownership if
  // -sil-verify-all is set.
  //
  // NOTE: We purposely return if we do can not look up a module here to ensure
  // that if we run into something that we do not understand, we do not assert
  // in user code even tohugh we aren't going to actually verify (the default
  // behavior when -sil-verify-all is disabled).
  auto *mod = Value->getModule();
  if (!mod || !mod->getOptions().VerifyAll)
    return;
#endif

  // Make sure that we are not a value of an instruction in a SILGlobalVariable
  // block.
  if (auto *definingInst = getDefiningInstruction()) {
    if (definingInst->isStaticInitializerInst()) {
      return;
    }
  }

  // If we are testing the verifier, bail so we only print errors once when
  // performing a full verification a function at a time by the
  // OwnershipVerifierStateDumper pass, instead of additionally in the
  // SILBuilder and in the actual SIL verifier that may be run by sil-opt.
  if (IsSILOwnershipVerifierTestingEnabled)
    return;

  // Since we do not have SILUndef, we now know that getFunction() should return
  // a real function. Assert in case this assumption is no longer true.
  auto *f = (*this)->getFunction();
  assert(f && "Instructions and arguments should have a function");

  using BehaviorKind = LinearLifetimeChecker::ErrorBehaviorKind;
  LinearLifetimeChecker::ErrorBuilder errorBuilder(
      *f, BehaviorKind::PrintMessageAndAssert);
  verifySILValueHelper(f, *this, errorBuilder, deadEndBlocks);
}

void SILFunction::verifyOwnership(DeadEndBlocks *deadEndBlocks) const {
  if (DisableOwnershipVerification)
    return;

#ifdef NDEBUG
  // When compiling without asserts enabled, only verify ownership if
  // -sil-verify-all is set.
  //
  // NOTE: We purposely return if we do can not look up a module here to ensure
  // that if we run into something that we do not understand, we do not assert
  // in user code even tohugh we aren't going to actually verify (the default
  // behavior when -sil-verify-all is disabled).
  auto *mod = &getModule();
  if (!mod || !mod->getOptions().VerifyAll)
    return;
#endif

  // If the given function has unqualified ownership or we have been asked by
  // the user not to verify this function, there is nothing to verify.
  if (!hasOwnership() || !shouldVerifyOwnership())
    return;

  using BehaviorKind = LinearLifetimeChecker::ErrorBehaviorKind;
  unsigned errorCounter = 0;
  Optional<LinearLifetimeChecker::ErrorBuilder> errorBuilder;
  if (IsSILOwnershipVerifierTestingEnabled) {
    errorBuilder.emplace(*this, BehaviorKind::PrintMessageAndReturnFalse,
                         &errorCounter);
  } else {
    errorBuilder.emplace(*this, BehaviorKind::PrintMessageAndAssert);
  }

  for (auto &block : *this) {
    for (auto *arg : block.getArguments()) {
      LinearLifetimeChecker::ErrorBuilder newBuilder = *errorBuilder;
      verifySILValueHelper(this, arg, newBuilder, deadEndBlocks);
    }

    for (auto &inst : block) {
      for (auto result : inst.getResults()) {
        LinearLifetimeChecker::ErrorBuilder newBuilder = *errorBuilder;
        verifySILValueHelper(this, result, newBuilder, deadEndBlocks);
      }
    }
  }
}
