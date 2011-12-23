//===- PrettyStackTrace.h - Crash trace information -----------------------===//
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
//
// This file defines RAII classes that give better dagnostic output
// about when, exactly, a crash is occurring.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRETTYSTACKTRACE_H
#define SWIFT_PRETTYSTACKTRACE_H

#include "llvm/Support/PrettyStackTrace.h"

namespace swift {
  class Decl;

/// PrettyStackTraceDecl - Observe that we are processing a specific
/// declaration.
class PrettyStackTraceDecl : public llvm::PrettyStackTraceEntry {
  Decl *TheDecl;
  const char *What;
public:
  PrettyStackTraceDecl(const char *what, Decl *D) : TheDecl(D), What(what) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

} // end namespace swift

#endif
