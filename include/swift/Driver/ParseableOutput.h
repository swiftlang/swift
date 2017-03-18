//===--- ParseableOutput.h - Helpers for parseable output -------*- C++ -*-===//
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
///
/// \file
/// \brief Helpers for emitting the driver's parseable output.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_PARSEABLEOUTPUT_H
#define SWIFT_DRIVER_PARSEABLEOUTPUT_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/TaskQueue.h"

namespace swift {
namespace driver {

class Job;

namespace parseable_output {

using swift::sys::ProcessId;
using swift::sys::ResourceStats;

/// Some single-threaded (cheaper than llvm::Statistic), always-on (not
/// controlled by the assertions build setting) statistics that are "always on"
/// in the driver, and reported as part of -parseable-output, for potential
/// consumption by external telemetry.
struct CompilationCounters {
  int64_t JobsTotal;
  int64_t JobsSkipped;

  int64_t DepCascadingTopLevel;
  int64_t DepCascadingDynamic;
  int64_t DepCascadingNominal;
  int64_t DepCascadingMember;
  int64_t DepCascadingExternal;
  int64_t DepTopLevel;
  int64_t DepDynamic;
  int64_t DepNominal;
  int64_t DepMember;
  int64_t DepExternal;
};

/// \brief Emits a "began" message to the given stream.
void emitBeganMessage(raw_ostream &os, const Job &Cmd, ProcessId Pid);

/// \brief Emits a "finished" message to the given stream.
void emitFinishedMessage(raw_ostream &os, const Job &Cmd, ProcessId Pid,
                         int ExitStatus, StringRef Output,
                         Optional<ResourceStats> Resources);

/// \brief Emits a "signalled" message to the given stream.
void emitSignalledMessage(raw_ostream &os, const Job &Cmd, ProcessId Pid,
                          StringRef ErrorMsg, StringRef Output, Optional<int> Signal,
                          Optional<ResourceStats> Resources);

/// \brief Emits a "skipped" message to the given stream.
void emitSkippedMessage(raw_ostream &os, const Job &Cmd);

/// \brief Emits a "compilation" message for the Driver job as a whole.
void emitCompilationMessage(raw_ostream &os, StringRef Name,
                            const CompilationCounters &Counters,
                            Optional<ResourceStats> Resources);

} // end namespace parseable_output
} // end namespace driver
} // end namespace swift

#endif
