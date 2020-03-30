//===--- LinearLifetimeCheckerPrivate.h -----------------------------------===//
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

#ifndef SWIFT_SIL_LINEARLIFETIMECHECKER_PRIVATE_H
#define SWIFT_SIL_LINEARLIFETIMECHECKER_PRIVATE_H

#include "swift/SIL/LinearLifetimeChecker.h"

namespace swift {

struct LinearLifetimeChecker::ErrorBehaviorKind {
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

class LinearLifetimeChecker::Error {
  ErrorBehaviorKind errorBehavior;
  bool foundUseAfterFree = false;
  bool foundLeak = false;
  bool foundOverConsume = false;

public:
  Error(ErrorBehaviorKind errorBehavior) : errorBehavior(errorBehavior) {}

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

} // namespace swift

#endif
