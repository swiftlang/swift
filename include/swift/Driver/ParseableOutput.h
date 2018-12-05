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
/// Helpers for emitting the driver's parseable output.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_PARSEABLEOUTPUT_H
#define SWIFT_DRIVER_PARSEABLEOUTPUT_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/TaskQueue.h"

namespace swift {
namespace driver {

class Job;

namespace parseable_output {

/// Emits a "began" message to the given stream.
void emitBeganMessage(raw_ostream &os, const Job &Cmd, int64_t Pid,
                      sys::TaskProcessInformation ProcInfo);

/// Emits a "finished" message to the given stream.
void emitFinishedMessage(raw_ostream &os, const Job &Cmd, int64_t Pid,
                         int ExitStatus, StringRef Output,
                         sys::TaskProcessInformation ProcInfo);

/// Emits a "signalled" message to the given stream.
void emitSignalledMessage(raw_ostream &os, const Job &Cmd, int64_t Pid,
                          StringRef ErrorMsg, StringRef Output,
                          Optional<int> Signal,
                          sys::TaskProcessInformation ProcInfo);

/// Emits a "skipped" message to the given stream.
void emitSkippedMessage(raw_ostream &os, const Job &Cmd);

} // end namespace parseable_output
} // end namespace driver
} // end namespace swift

#endif
