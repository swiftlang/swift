//===--- PrettyStackTrace.cpp - Generic PrettyStackTraceEntries -----------===//
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
//
//  This file implements several PrettyStackTraceEntries that probably
//  ought to be in LLVM.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/QuotedString.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

void PrettyStackTraceStringAction::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ' << QuotedString(TheString) << '\n';
}

void PrettyStackTraceFileContents::print(llvm::raw_ostream &out) const {
  out << "Contents of " << Buffer.getBufferIdentifier() << ":\n---\n"
      << Buffer.getBuffer();
  if (!Buffer.getBuffer().ends_with("\n"))
    out << '\n';
  out << "---\n";
}

void PrettyStackTraceSwiftVersion::print(llvm::raw_ostream &out) const {
  out << version::getSwiftFullVersion() << '\n';
}

void swift::abortWithPrettyStackTraceMessage(
    llvm::function_ref<void(llvm::raw_ostream &)> message) {
  llvm::SmallString<0> errorStr;
  llvm::raw_svector_ostream out(errorStr);
  message(out);
  llvm::PrettyStackTraceString trace(errorStr.c_str());
  abort();
}

void swift::abortWithPrettyStackTraceMessage(StringRef message) {
  auto messageStr = message.str();
  llvm::PrettyStackTraceString trace(messageStr.c_str());
  abort();
}
