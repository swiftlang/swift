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

/// Returns true if:
///
/// 1. No consuming uses are reachable from any other consuming use, from any
/// non-consuming uses, or from the producer instruction.
/// 2. The consuming use set jointly post dominates producers and all non
/// consuming uses.
///
/// \p value The value whose lifetime we are checking.
/// \p consumingUses the array of users that destroy or consume a value.
/// \p nonConsumingUses regular uses
/// \p deadEndBlocks a cache for the dead end block computation
/// \p errorBehavior If we detect an error, should we return false or hard
/// error.
/// \p leakingBlocks If non-null a list of blocks where the value was detected
/// to leak. Can be used to insert missing destroys.
LinearLifetimeError valueHasLinearLifetime(
    SILValue value, ArrayRef<BranchPropagatedUser> consumingUses,
    ArrayRef<BranchPropagatedUser> nonConsumingUses,
    SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks,
    DeadEndBlocks &deadEndBlocks, ownership::ErrorBehaviorKind errorBehavior,
    SmallVectorImpl<SILBasicBlock *> *leakingBlocks = nullptr);

/// Returns true if v is an address or trivial.
bool isValueAddressOrTrivial(SILValue v, SILModule &m);

/// These operations forward both owned and guaranteed ownership.
bool isOwnershipForwardingValueKind(SILNodeKind kind);

/// These operations forward guaranteed ownership, but don't necessarily forward
/// owned values.
bool isGuaranteedForwardingValueKind(SILNodeKind kind);

bool isGuaranteedForwardingValue(SILValue value);

bool isOwnershipForwardingInst(SILInstruction *i);

bool isGuaranteedForwardingInst(SILInstruction *i);

/// Look up through the def-use chain of \p inputValue, recording any "borrow"
/// introducers that we find into \p out.
bool getUnderlyingBorrowIntroducers(SILValue inputValue,
                                    SmallVectorImpl<SILValue> &out);

} // namespace swift

#endif
