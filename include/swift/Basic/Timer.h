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
  ///
  /// Please don't use this class directly for anything other than the flat,
  /// top-level compilation-phase timing numbers; unadorned SharedTimers are
  /// enabled, summed and reported via -debug-time-compilation, using LLVM's
  /// built-in logic for timer groups, and that logic doesn't work right if
  /// there's any nesting or reentry in timers at all (crashes on reentry,
  /// simply mis-reports nesting). Additional SharedTimers also confuse users
  /// who are expecting to see only top-level phase timings when they pass
  /// -debug-time-compilation.
  ///
  /// Instead, please use FrontendStatsTracer objects and the -stats-output-dir
  /// subsystem in include/swift/Basic/Statistic.h. In addition to not
  /// interfering with users passing -debug-time-compilation, the
  /// FrontendStatsTracer objects automatically instantiate nesting-safe and
  /// reentry-safe SharedTimers themselves, as well as supporting event and
  /// source-entity tracing and profiling.
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
} // end namespace swift

#endif // SWIFT_BASIC_TIMER_H
