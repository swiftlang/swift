//===--- OwnershipUtils.h ------------------------------------*- C++ -*----===//
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

#ifndef SWIFT_SIL_OWNERSHIPUTILS_H
#define SWIFT_SIL_OWNERSHIPUTILS_H

#include "swift/Basic/LLVM.h"
#include "swift/SIL/BranchPropagatedUser.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SILBasicBlock;
class SILInstruction;
class SILModule;
class SILValue;
class DeadEndBlocks;
class BranchPropagatedUser;

namespace ownership {

struct ErrorBehaviorKind {
  enum inner_t {
    Invalid = 0,
    ReturnFalse = 1,
    PrintMessage = 2,
    Assert = 4,
    ReturnFalseOnLeak = 8,
    PrintMessageAndReturnFalse = PrintMessage | ReturnFalse,
    PrintMessageAndAssert = PrintMessage | Assert,
    ReturnFalseOnLeakAssertOtherwise = ReturnFalseOnLeak | Assert,
  } Value;

  ErrorBehaviorKind() : Value(Invalid) {}
  ErrorBehaviorKind(inner_t Inner) : Value(Inner) { assert(Value != Invalid); }

  bool shouldAssert() const {
    assert(Value != Invalid);
    return Value & Assert;
  }

  bool shouldReturnFalseOnLeak() const {
    assert(Value != Invalid);
    return Value & ReturnFalseOnLeak;
  }

  bool shouldPrintMessage() const {
    assert(Value != Invalid);
    return Value & PrintMessage;
  }

  bool shouldReturnFalse() const {
    assert(Value != Invalid);
    return Value & ReturnFalse;
  }
};

} // end namespace ownership

class LinearLifetimeError {
  ownership::ErrorBehaviorKind errorBehavior;
  bool foundUseAfterFree = false;
  bool foundLeak = false;
  bool foundOverConsume = false;

public:
  LinearLifetimeError(ownership::ErrorBehaviorKind errorBehavior)
      : errorBehavior(errorBehavior) {}

  bool getFoundError() const {
    return foundUseAfterFree || foundLeak || foundOverConsume;
  }

  bool getFoundLeak() const { return foundLeak; }

  bool getFoundUseAfterFree() const { return foundUseAfterFree; }

  bool getFoundOverConsume() const { return foundOverConsume; }

  void handleLeak(llvm::function_ref<void()> &&messagePrinterFunc) {
    foundLeak = true;

    if (errorBehavior.shouldPrintMessage())
      messagePrinterFunc();

    if (errorBehavior.shouldReturnFalseOnLeak())
      return;

    // We already printed out our error if we needed to, so don't pass it along.
    handleError([]() {});
  }

  void handleOverConsume(llvm::function_ref<void()> &&messagePrinterFunc) {
    foundOverConsume = true;
    handleError(std::move(messagePrinterFunc));
  }

  void handleUseAfterFree(llvm::function_ref<void()> &&messagePrinterFunc) {
    foundUseAfterFree = true;
    handleError(std::move(messagePrinterFunc));
  }

private:
  void handleError(llvm::function_ref<void()> &&messagePrinterFunc) {
    if (errorBehavior.shouldPrintMessage())
      messagePrinterFunc();

    if (errorBehavior.shouldReturnFalse()) {
      return;
    }

    assert(errorBehavior.shouldAssert() && "At this point, we should assert");
    llvm_unreachable("triggering standard assertion failure routine");
  }
};

/// A class used to validate linear lifetime with respect to an SSA-like
/// definition.
///
/// This class is able to both validate that a linear lifetime has been properly
/// constructed (for verification and safety purposes) as well as return to the
/// caller upon failure, what the failure was. In certain cases (for instance if
/// there exists a path without a non-consuming use), the class will report back
/// the specific insertion points needed to insert these compensating releases.
///
/// DISCUSSION: A linear lifetime consists of a starting block or instruction
/// and a list of non-consuming uses and a set of consuming uses. The consuming
/// uses must not be reachable from each other and jointly post-dominate all
/// consuming uses as well as the defining block/instruction.
class LinearLifetimeChecker {
  SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks;
  DeadEndBlocks &deadEndBlocks;

public:
  LinearLifetimeChecker(SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks,
                        DeadEndBlocks &deadEndBlocks)
      : visitedBlocks(visitedBlocks), deadEndBlocks(deadEndBlocks) {}

  /// Returns true if:
  ///
  /// 1. No consuming uses are reachable from any other consuming use, from any
  /// non-consuming uses, or from the producer instruction.
  /// 2. The consuming use set jointly post dominates producers and all non
  /// consuming uses.
  ///
  /// Returns false otherwise.
  ///
  /// \p value The value whose lifetime we are checking.
  /// \p consumingUses the array of users that destroy or consume a value.
  /// \p nonConsumingUses regular uses
  /// \p errorBehavior If we detect an error, should we return false or hard
  /// error.
  /// \p leakingBlocks If non-null a list of blocks where the value was detected
  /// to leak. Can be used to insert missing destroys.
  LinearLifetimeError
  checkValue(SILValue value, ArrayRef<BranchPropagatedUser> consumingUses,
             ArrayRef<BranchPropagatedUser> nonConsumingUses,
             ownership::ErrorBehaviorKind errorBehavior,
             SmallVectorImpl<SILBasicBlock *> *leakingBlocks = nullptr);

  /// Returns true that \p value forms a linear lifetime with consuming uses \p
  /// consumingUses, non consuming uses \p nonConsumingUses. Returns false
  /// otherwise.
  bool validateLifetime(SILValue value,
                        ArrayRef<BranchPropagatedUser> consumingUses,
                        ArrayRef<BranchPropagatedUser> nonConsumingUses) {
    return !checkValue(value, consumingUses, nonConsumingUses,
                       ownership::ErrorBehaviorKind::ReturnFalse,
                       nullptr /*leakingBlocks*/)
                .getFoundError();
  }
};

/// Returns true if v is an address or trivial.
bool isValueAddressOrTrivial(SILValue v);

/// These operations forward both owned and guaranteed ownership.
bool isOwnershipForwardingValueKind(SILNodeKind kind);

/// These operations forward guaranteed ownership, but don't necessarily forward
/// owned values.
bool isGuaranteedForwardingValueKind(SILNodeKind kind);

bool isGuaranteedForwardingValue(SILValue value);

bool isOwnershipForwardingInst(SILInstruction *i);

bool isGuaranteedForwardingInst(SILInstruction *i);

struct BorrowScopeOperandKind {
  using UnderlyingKindTy = std::underlying_type<SILInstructionKind>::type;

  enum Kind : UnderlyingKindTy {
    BeginBorrow = UnderlyingKindTy(SILInstructionKind::BeginBorrowInst),
    BeginApply = UnderlyingKindTy(SILInstructionKind::BeginApplyInst),
  };

  Kind value;

  BorrowScopeOperandKind(Kind newValue) : value(newValue) {}
  BorrowScopeOperandKind(const BorrowScopeOperandKind &other)
      : value(other.value) {}
  operator Kind() const { return value; }

  static Optional<BorrowScopeOperandKind> get(SILInstructionKind kind) {
    switch (kind) {
    default:
      return None;
    case SILInstructionKind::BeginBorrowInst:
      return BorrowScopeOperandKind(BeginBorrow);
    case SILInstructionKind::BeginApplyInst:
      return BorrowScopeOperandKind(BeginApply);
    }
  }

  void print(llvm::raw_ostream &os) const;
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in the debugger");
};

/// An operand whose user instruction introduces a new borrow scope for the
/// operand's value. The value of the operand must be considered as implicitly
/// borrowed until the user's corresponding end scope instruction.
struct BorrowScopeOperand {
  BorrowScopeOperandKind kind;
  Operand *op;

  BorrowScopeOperand(Operand *op)
      : kind(*BorrowScopeOperandKind::get(op->getUser()->getKind())), op(op) {}

  /// If value is a borrow introducer return it after doing some checks.
  static Optional<BorrowScopeOperand> get(Operand *op) {
    auto *user = op->getUser();
    auto kind = BorrowScopeOperandKind::get(user->getKind());
    if (!kind)
      return None;
    return BorrowScopeOperand(*kind, op);
  }

  void visitEndScopeInstructions(function_ref<void(Operand *)> func) const {
    switch (kind) {
    case BorrowScopeOperandKind::BeginBorrow:
      for (auto *use : cast<BeginBorrowInst>(op->getUser())->getUses()) {
        if (isa<EndBorrowInst>(use->getUser())) {
          func(use);
        }
      }
      return;
    case BorrowScopeOperandKind::BeginApply: {
      auto *user = cast<BeginApplyInst>(op->getUser());
      for (auto *use : user->getTokenResult()->getUses()) {
        func(use);
      }
      return;
    }
    }
    llvm_unreachable("Covered switch isn't covered");
  }

private:
  /// Internal constructor for failable static constructor. Please do not expand
  /// its usage since it assumes the code passed in is well formed.
  BorrowScopeOperand(BorrowScopeOperandKind kind, Operand *op)
      : kind(kind), op(op) {}
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              BorrowScopeOperandKind kind);

struct BorrowScopeIntroducingValueKind {
  using UnderlyingKindTy = std::underlying_type<ValueKind>::type;

  /// Enum we use for exhaustive pattern matching over borrow scope introducers.
  enum Kind : UnderlyingKindTy {
    LoadBorrow = UnderlyingKindTy(ValueKind::LoadBorrowInst),
    BeginBorrow = UnderlyingKindTy(ValueKind::BeginBorrowInst),
    SILFunctionArgument = UnderlyingKindTy(ValueKind::SILFunctionArgument),
  };

  static Optional<BorrowScopeIntroducingValueKind> get(ValueKind kind) {
    switch (kind) {
    default:
      return None;
    case ValueKind::LoadBorrowInst:
      return BorrowScopeIntroducingValueKind(LoadBorrow);
    case ValueKind::BeginBorrowInst:
      return BorrowScopeIntroducingValueKind(BeginBorrow);
    case ValueKind::SILFunctionArgument:
      return BorrowScopeIntroducingValueKind(SILFunctionArgument);
    }
  }

  Kind value;

  BorrowScopeIntroducingValueKind(Kind newValue) : value(newValue) {}
  BorrowScopeIntroducingValueKind(const BorrowScopeIntroducingValueKind &other)
      : value(other.value) {}
  operator Kind() const { return value; }

  /// Is this a borrow scope that begins and ends within the same function and
  /// thus is guaranteed to have an "end_scope" instruction.
  ///
  /// In contrast, borrow scopes that are non-local (e.x. from
  /// SILFunctionArguments) rely a construct like a SILFunction as the begin/end
  /// of the scope.
  bool isLocalScope() const {
    switch (value) {
    case BorrowScopeIntroducingValueKind::BeginBorrow:
    case BorrowScopeIntroducingValueKind::LoadBorrow:
      return true;
    case BorrowScopeIntroducingValueKind::SILFunctionArgument:
      return false;
    }
    llvm_unreachable("Covered switch isnt covered?!");
  }

  void print(llvm::raw_ostream &os) const;
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in the debugger");
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              BorrowScopeIntroducingValueKind kind);

/// A higher level construct for working with values that represent the
/// introduction of a new borrow scope.
///
/// DISCUSSION: A "borrow introducer" is a SILValue that represents the
/// beginning of a borrow scope that the ownership verifier validates. The idea
/// is this API allows one to work in a generic way with all of the various
/// introducers.
///
/// Some examples of borrow introducers: guaranteed SILFunctionArgument,
/// LoadBorrow, BeginBorrow, guaranteed BeginApply results.
///
/// NOTE: It is assumed that if a borrow introducer is a value of a
/// SILInstruction with multiple results, that the all of the SILInstruction's
/// guaranteed results are borrow introducers. In practice this means that
/// borrow introducers can not have guaranteed results that are not creating a
/// new borrow scope. No such instructions exist today.
struct BorrowScopeIntroducingValue {
  BorrowScopeIntroducingValueKind kind;
  SILValue value;

  BorrowScopeIntroducingValue(LoadBorrowInst *lbi)
      : kind(BorrowScopeIntroducingValueKind::LoadBorrow), value(lbi) {}
  BorrowScopeIntroducingValue(BeginBorrowInst *bbi)
      : kind(BorrowScopeIntroducingValueKind::BeginBorrow), value(bbi) {}
  BorrowScopeIntroducingValue(SILFunctionArgument *arg)
      : kind(BorrowScopeIntroducingValueKind::SILFunctionArgument), value(arg) {
    assert(arg->getOwnershipKind() == ValueOwnershipKind::Guaranteed);
  }

  BorrowScopeIntroducingValue(SILValue v)
      : kind(*BorrowScopeIntroducingValueKind::get(v->getKind())), value(v) {
    assert(v.getOwnershipKind() == ValueOwnershipKind::Guaranteed);
  }

  /// If value is a borrow introducer return it after doing some checks.
  static Optional<BorrowScopeIntroducingValue> get(SILValue value) {
    auto kind = BorrowScopeIntroducingValueKind::get(value->getKind());
    if (!kind || value.getOwnershipKind() != ValueOwnershipKind::Guaranteed)
      return None;
    return BorrowScopeIntroducingValue(*kind, value);
  }

  /// If this value is introducing a local scope, gather all local end scope
  /// instructions and append them to \p scopeEndingInsts. Asserts if this is
  /// called with a scope that is not local.
  ///
  /// NOTE: To determine if a scope is a local scope, call
  /// BorrowScopeIntoducingValue::isLocalScope().
  void getLocalScopeEndingInstructions(
      SmallVectorImpl<SILInstruction *> &scopeEndingInsts) const;

  /// If this value is introducing a local scope, gather all local end scope
  /// instructions and pass them individually to visitor. Asserts if this is
  /// called with a scope that is not local.
  ///
  /// The intention is that this method can be used instead of
  /// BorrowScopeIntroducingValue::getLocalScopeEndingUses() to avoid
  /// introducing an intermediate array when one needs to transform the
  /// instructions before storing them.
  ///
  /// NOTE: To determine if a scope is a local scope, call
  /// BorrowScopeIntoducingValue::isLocalScope().
  void visitLocalScopeEndingUses(function_ref<void(Operand *)> visitor) const;

  bool isLocalScope() const { return kind.isLocalScope(); }

  /// Returns true if the passed in set of instructions is completely within the
  /// lifetime of this borrow introducer.
  ///
  /// NOTE: Scratch space is used internally to this method to store the end
  /// borrow scopes if needed.
  bool areInstructionsWithinScope(
      ArrayRef<BranchPropagatedUser> instructions,
      SmallVectorImpl<BranchPropagatedUser> &scratchSpace,
      SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks,
      DeadEndBlocks &deadEndBlocks) const;

private:
  /// Internal constructor for failable static constructor. Please do not expand
  /// its usage since it assumes the code passed in is well formed.
  BorrowScopeIntroducingValue(BorrowScopeIntroducingValueKind kind,
                              SILValue value)
      : kind(kind), value(value) {}
};

/// Look up through the def-use chain of \p inputValue, recording any "borrow"
/// introducing values that we find into \p out. If at any point, we find a
/// point in the chain we do not understand, we bail and return false. If we are
/// able to understand all of the def-use graph, we know that we have found all
/// of the borrow introducing values, we return true.
bool getUnderlyingBorrowIntroducingValues(
    SILValue inputValue, SmallVectorImpl<BorrowScopeIntroducingValue> &out);

} // namespace swift

#endif
