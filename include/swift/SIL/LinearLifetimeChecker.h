//===--- LinearLifetimeChecker.h ------------------------------------------===//
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

#ifndef SWIFT_SIL_LINEARLIFETIMECHECKER_H
#define SWIFT_SIL_LINEARLIFETIMECHECKER_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
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
  checkValue(SILValue value, ArrayRef<Operand *> consumingUses,
             ArrayRef<Operand *> nonConsumingUses,
             ownership::ErrorBehaviorKind errorBehavior,
             SmallVectorImpl<SILBasicBlock *> *leakingBlocks = nullptr);

  /// Returns true that \p value forms a linear lifetime with consuming uses \p
  /// consumingUses, non consuming uses \p nonConsumingUses. Returns false
  /// otherwise.
  bool validateLifetime(SILValue value, ArrayRef<Operand *> consumingUses,
                        ArrayRef<Operand *> nonConsumingUses) {
    return !checkValue(value, consumingUses, nonConsumingUses,
                       ownership::ErrorBehaviorKind::ReturnFalse,
                       nullptr /*leakingBlocks*/)
                .getFoundError();
  }
};

} // namespace swift

#endif
