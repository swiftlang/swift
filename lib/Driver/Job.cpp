//===--- Job.cpp - Command to Execute -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::driver;

void CommandOutput::setAdditionalOutputForType(types::ID type,
                                               StringRef OutputFilename) {
  AdditionalOutputsMap[type] = OutputFilename;
}

const std::string &
CommandOutput::getAdditionalOutputForType(types::ID type) const {
  auto iter = AdditionalOutputsMap.find(type);
  if (iter != AdditionalOutputsMap.end())
    return iter->second;

  static const std::string empty;
  return empty;
}

const std::string &
CommandOutput::getAnyOutputForType(types::ID type) const {
  if (PrimaryOutputType == type)
    return PrimaryOutputFilenames[0];
  return getAdditionalOutputForType(type);
}

static void escapeAndPrintString(llvm::raw_ostream &os, StringRef Str) {
  if (Str.empty()) {
    // Special-case the empty string.
    os << "\"\"";
    return;
  }

  bool NeedsEscape = Str.find_first_of(" \"\\$") != StringRef::npos;

  if (!NeedsEscape) {
    // This string doesn't have anything we need to escape, so print it directly
    os << Str;
    return;
  }

  // Quote and escape. This isn't really complete, but is good enough, and
  // matches how Clang's Command handles escaping arguments.
  os << '"';
  for (const char c : Str) {
    switch (c) {
    case '"':
    case '\\':
    case '$':
      // These characters need to be escaped.
      os << '\\';
      // Fall-through to the default case, since we still need to print the
      // character.
      SWIFT_FALLTHROUGH;
    default:
      os << c;
    }
  }
  os << '"';
}

void Job::printArguments(raw_ostream &os,
                         const llvm::opt::ArgStringList &Args) {
  interleave(Args,
             [&](const char *Arg) { escapeAndPrintString(os, Arg); },
             [&] { os << ' '; });
}

void Job::printCommandLine(raw_ostream &os, StringRef Terminator) const {
  escapeAndPrintString(os, Executable);
  os << ' ';
  printArguments(os, Arguments);
  os << Terminator;
}
