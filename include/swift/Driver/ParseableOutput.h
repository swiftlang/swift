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

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/TaskQueue.h"

namespace swift {
namespace driver {

namespace parseable_output {

struct CommandInput {
  std::string Path;
  CommandInput() {}
  CommandInput(StringRef Path) : Path(Path) {}
};

using OutputPair = std::pair<file_types::ID, std::string>;

/// Information about a job to be reported.
struct JobInfo {
  std::string Executable;
  SmallVector<std::string, 16> Arguments;
  std::string CommandLine;
  SmallVector<CommandInput, 4> Inputs;
  SmallVector<OutputPair, 8> Outputs;
};

/// Emits a "began" message to the given stream.
void emitBeganMessage(
    raw_ostream &os, StringRef name, const JobInfo &info, int64_t pid,
    sys::TaskProcessInformation procInfo);

/// Emits a "finished" message to the given stream.
void emitFinishedMessage(
    raw_ostream &os, StringRef name, const JobInfo &info, int64_t pid,
    int exitStatus, StringRef output,
    sys::TaskProcessInformation procInfo);

/// Emits a "signalled" message to the given stream.
void emitSignalledMessage(
    raw_ostream &os, StringRef name, const JobInfo &info, int64_t pid,
    StringRef errorMsg, StringRef output,
    Optional<int> signal,
    sys::TaskProcessInformation procInfo);

/// Emits a "skipped" message to the given stream.
void emitSkippedMessage(raw_ostream &os, StringRef name, const JobInfo &info);

} // end namespace parseable_output
} // end namespace driver
} // end namespace swift

#endif
