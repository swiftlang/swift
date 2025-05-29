//===--- Program.h ----------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_PROGRAM_H
#define SWIFT_BASIC_PROGRAM_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/Program.h"
#include <optional>

namespace swift {

/// This function executes the program using the arguments provided,
/// preferring to reexecute the current process, if supported.
///
/// \param Program Path of the program to be executed
/// \param args A vector of strings that are passed to the program. The first
/// element should be the name of the program. The list *must* be terminated by
/// a null char * entry.
/// \param env An optional vector of strings to use for the program's
/// environment. If not provided, the current program's environment will be
/// used.
///
/// \returns Typically, this function will not return, as the current process
/// will no longer exist, or it will call exit() if the program was successfully
/// executed. In the event of an error, this function will return a negative
/// value indicating a failure to execute.
int ExecuteInPlace(const char *Program, const char **args,
                   const char **env = nullptr);

struct ChildProcessInfo {
  llvm::sys::ProcessInfo ProcessInfo;
  int Write;
  int Read;

  ChildProcessInfo(llvm::sys::ProcessInfo ProcessInfo, int Write, int Read)
      : ProcessInfo(ProcessInfo), Write(Write), Read(Read) {}
};

/// This function executes the program using the argument provided.
/// Establish pipes between the current process and the spawned process.
/// Returned a \c ChildProcessInfo where WriteFileDescriptor is piped to
/// child's STDIN, and ReadFileDescriptor is piped from child's STDOUT.
///
/// \param program Path of the program to be executed
/// \param args An array of strings that are passed to the program. The first
/// element should be the name of the program.
/// \param env An optional array of 'key=value 'strings to use for the program's
/// environment.
llvm::ErrorOr<swift::ChildProcessInfo> ExecuteWithPipe(
    llvm::StringRef program, llvm::ArrayRef<llvm::StringRef> args,
    std::optional<llvm::ArrayRef<llvm::StringRef>> env = std::nullopt);

} // end namespace swift

#endif // SWIFT_BASIC_PROGRAM_H
