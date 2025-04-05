//===--- PrettyStackTrace.h - Generic stack-trace prettifiers ---*- C++ -*-===//
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

#ifndef SWIFT_BASIC_PRETTYSTACKTRACE_H
#define SWIFT_BASIC_PRETTYSTACKTRACE_H

#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class MemoryBuffer;
}

namespace swift {

/// A PrettyStackTraceEntry for performing an action involving a StringRef.
///
/// The message is:
///   While <action> "<string>"\n
class PrettyStackTraceStringAction : public llvm::PrettyStackTraceEntry {
  const char *Action;
  llvm::StringRef TheString;
public:
  PrettyStackTraceStringAction(const char *action, llvm::StringRef string)
    : Action(action), TheString(string) {}
  void print(llvm::raw_ostream &OS) const override;
};

/// A PrettyStackTraceEntry to dump the contents of a file.
class PrettyStackTraceFileContents : public llvm::PrettyStackTraceEntry {
  const llvm::MemoryBuffer &Buffer;
public:
  explicit PrettyStackTraceFileContents(const llvm::MemoryBuffer &buffer)
    : Buffer(buffer) {}
  void print(llvm::raw_ostream &OS) const override;
};

/// A PrettyStackTraceEntry to print the version of the compiler.
class PrettyStackTraceSwiftVersion : public llvm::PrettyStackTraceEntry {
public:
  void print(llvm::raw_ostream &OS) const override;
};

/// Aborts the program, printing a given message to a PrettyStackTrace frame
/// before exiting.
[[noreturn]]
void abortWithPrettyStackTraceMessage(
    llvm::function_ref<void(llvm::raw_ostream &)> message);

/// Aborts the program, printing a given message to a PrettyStackTrace frame
/// before exiting.
[[noreturn]]
void abortWithPrettyStackTraceMessage(llvm::StringRef message);

} // end namespace swift

#endif // SWIFT_BASIC_PRETTYSTACKTRACE_H
