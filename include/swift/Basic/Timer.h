//===--- Timer.h - Shared timers for compilation phases ---------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_TIMER_H
#define SWIFT_BASIC_TIMER_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Timer.h"

namespace swift {
  /// A convenience class for declaring a timer that's part of the Swift
  /// compilation timers group.
  class SharedTimer {
    enum class State {
      Initial,
      Skipped,
      Enabled
    };
    static State CompilationTimersEnabled;

    Optional<llvm::NamedRegionTimer> Timer;

  public:
    explicit SharedTimer(StringRef name) {
      if (CompilationTimersEnabled == State::Enabled)
        Timer.emplace(name, name, "swift", "Swift compilation");
      else
        CompilationTimersEnabled = State::Skipped;
    }

    /// Must be called before any SharedTimers have been created.
    static void enableCompilationTimers() {
      assert(CompilationTimersEnabled != State::Skipped &&
             "a timer has already been created");
      CompilationTimersEnabled = State::Enabled;
    }
  };

  /// A SharedTimer for recursive routines.
  /// void example() {
  ///  RecursiveSharedTimer::Guard guard; // MUST BE AT TOP SCOPE of function to
  ///  work right! if (auto s = getASTContext().Stats) {
  ///    guard =
  ///    ctx.Stats->getFrontendRecursiveSharedTimers().NominalTypeDecl__lookupDirect.getGuard();
  //  }
  ///   ...
  /// }

  class RecursiveSharedTimer {
  private:
    int recursionCount = 0;
    const StringRef name;
    llvm::Optional<SharedTimer> timer;

    void enterRecursiveFunction() {
      assert(recursionCount >= 0  &&  "too many exits");
      if (recursionCount++ == 0)
        timer.emplace(name);
    }
    void exitRecursiveFunction() {
      assert(recursionCount > 0  &&  "too many exits");
      if (--recursionCount == 0)
        timer.reset();
    }

  public:
    RecursiveSharedTimer(StringRef name) : name(name) {}

    struct Guard {
      RecursiveSharedTimer *recursiveTimerOrNull;

      Guard(RecursiveSharedTimer *rst) : recursiveTimerOrNull(rst) {
        if (recursiveTimerOrNull)
          recursiveTimerOrNull->enterRecursiveFunction();
      }
      ~Guard() {
        if (recursiveTimerOrNull)
          recursiveTimerOrNull->exitRecursiveFunction();
      }

      // All this stuff is to do an RAII object that be moved.
      Guard() : recursiveTimerOrNull(nullptr) {}
      Guard(Guard &&other) {
        recursiveTimerOrNull = other.recursiveTimerOrNull;
        other.recursiveTimerOrNull = nullptr;
      }
      Guard &operator=(Guard &&other) {
        recursiveTimerOrNull = other.recursiveTimerOrNull;
        other.recursiveTimerOrNull = nullptr;
        return *this;
      }
      Guard(const Guard &) = delete;
      Guard &operator=(const Guard &) = delete;
    };

    Guard getGuard() { return Guard(this); }
  };
} // end namespace swift

#endif // SWIFT_BASIC_TIMER_H
