//===----- ParseableOutput.h - Helpers for parseable output ------- C++ -*-===//
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
///
/// \file
/// Helpers for emitting the compiler's parseable output.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSEABLEOUTPUT_H
#define SWIFT_PARSEABLEOUTPUT_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Driver/Job.h"
#include "swift/Frontend/Frontend.h"

namespace swift {

namespace parseable_output {

/// Quasi-PIDs are _negative_ PID-like unique keys used to
/// masquerade batch job constituents as (quasi)processes, when writing
/// parseable output to consumers that don't understand the idea of a batch
/// job. They are negative in order to avoid possibly colliding with real
/// PIDs (which are always positive). We start at -1000 here as a crude but
/// harmless hedge against colliding with an errno value that might slip
/// into the stream of real PIDs (say, due to a TaskQueue bug).
const int QUASI_PID_START = -1000;

/// Emits a "began" message to the given stream, corresponding to a Driver Job.
void emitBeganMessage(raw_ostream &os, const driver::Job &Cmd, int64_t Pid,
                      sys::TaskProcessInformation ProcInfo);

/// Emits a "began" message to the given stream, corresponding to a given
/// Frontend Compiler Invocation.
void emitBeganMessage(raw_ostream &os, const CompilerInvocation &Invocation,
                      ArrayRef<const char *> Args, int64_t OSPid);

/// Emits a "finished" message to the given stream.
void emitFinishedMessage(raw_ostream &os, const driver::Job &Cmd, int64_t Pid,
                         int ExitStatus, StringRef Output,
                         sys::TaskProcessInformation ProcInfo);

/// Emits a "finished" message to the given stream corresponding to a given
/// Frontend Compiler Invocation.
void emitFinishedMessage(
    raw_ostream &os, const CompilerInvocation &Invocation, int ExitStatus,
    const llvm::StringMap<std::vector<std::string>> &FileSpecificDiagnostics,
    int64_t OSPid);

/// Emits a "signalled" message to the given stream.
void emitSignalledMessage(raw_ostream &os, const driver::Job &Cmd, int64_t Pid,
                          StringRef ErrorMsg, StringRef Output,
                          Optional<int> Signal,
                          sys::TaskProcessInformation ProcInfo);

/// Emits a "skipped" message to the given stream.
void emitSkippedMessage(raw_ostream &os, const driver::Job &Cmd);

} // end namespace parseable_output
} // end namespace swift

#endif
