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
#include "llvm/Support/ErrorHandling.h"

namespace swift {

struct LLVM_LIBRARY_VISIBILITY LinearLifetimeChecker::ErrorBehaviorKind {
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

class LLVM_LIBRARY_VISIBILITY LinearLifetimeChecker::Error {
  friend class ErrorBuilder;

  bool foundUseAfterFree = false;
  bool foundLeak = false;
  bool foundOverConsume = false;

public:
  Error() {}

  bool getFoundError() const {
    return foundUseAfterFree || foundLeak || foundOverConsume;
  }

  bool getFoundLeak() const { return foundLeak; }

  bool getFoundUseAfterFree() const { return foundUseAfterFree; }

  bool getFoundOverConsume() const { return foundOverConsume; }
};

class LLVM_LIBRARY_VISIBILITY LinearLifetimeChecker::ErrorBuilder {
  StringRef functionName;
  ErrorBehaviorKind behavior;
  Optional<Error> error;

  // NOTE: This is only here so that we can emit a unique id for all errors to
  // ease working with FileCheck.
  static unsigned errorMessageCount;

public:
  ErrorBuilder(const SILFunction &fn,
               LinearLifetimeChecker::ErrorBehaviorKind behavior)
      : functionName(fn.getName()), behavior(behavior), error(Error()) {}

  ErrorBuilder(const SILFunction &fn,
               LinearLifetimeChecker::ErrorBehaviorKind::inner_t behavior)
      : functionName(fn.getName()), behavior(behavior), error(Error()) {}

  Error getFinalError() && {
    auto result = *error;
    error = None;
    return result;
  }

  bool handleLeak(llvm::function_ref<void()> &&messagePrinterFunc) {
    error->foundLeak = true;

    if (behavior.shouldPrintMessage()) {
      llvm::errs() << "Error#: " << errorMessageCount
                   << ". Begin Error in Function: '" << functionName << "'\n";
      messagePrinterFunc();
      llvm::errs() << "Error#: " << errorMessageCount
                   << ". End Error in Function: '" << functionName << "'\n";
      ++errorMessageCount;
    }

    if (behavior.shouldReturnFalseOnLeak()) {
      return false;
    }

    // We already printed out our error if we needed to, so don't pass it along.
    return handleError([]() {}, true);
  }

  bool handleOverConsume(llvm::function_ref<void()> &&messagePrinterFunc) {
    error->foundOverConsume = true;
    return handleError(std::move(messagePrinterFunc));
  }

  bool handleUseAfterFree(llvm::function_ref<void()> &&messagePrinterFunc) {
    error->foundUseAfterFree = true;
    return handleError(std::move(messagePrinterFunc));
  }

  bool
  handleMalformedSIL(llvm::function_ref<void()> &&messagePrinterFunc) const {
    return handleError(std::move(messagePrinterFunc));
  }

private:
  bool handleError(llvm::function_ref<void()> &&messagePrinterFunc,
                   bool quiet = false) const {
    if (behavior.shouldPrintMessage()) {
      if (!quiet) {
        llvm::errs() << "Error#: " << errorMessageCount
                     << ". Begin Error in Function: '" << functionName << "'\n";
      }
      messagePrinterFunc();
      if (!quiet) {
        llvm::errs() << "Error#: " << errorMessageCount
                     << ". End Error in Function: '" << functionName << "'\n";
        ++errorMessageCount;
      }
    }

    if (behavior.shouldReturnFalse()) {
      return false;
    }

    llvm::errs() << "Found ownership error?!\n";
    llvm::report_fatal_error("triggering standard assertion failure routine");
  }
};

} // namespace swift

#endif
