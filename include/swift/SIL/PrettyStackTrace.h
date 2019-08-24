//===--- PrettyStackTrace.h - Crash trace information -----------*- C++ -*-===//
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
// This file defines SIL-specific RAII classes that give better diagnostic
// output about when, exactly, a crash is occurring.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PRETTYSTACKTRACE_H
#define SWIFT_SIL_PRETTYSTACKTRACE_H

#include "swift/SIL/SILLocation.h"
#include "llvm/Support/PrettyStackTrace.h"

namespace swift {
class ASTContext;
class SILFunction;
class SILNode;

void printSILLocationDescription(llvm::raw_ostream &out, SILLocation loc,
                                 ASTContext &ctx);

/// PrettyStackTraceLocation - Observe that we are doing some
/// processing starting at a SIL location.
class PrettyStackTraceSILLocation : public llvm::PrettyStackTraceEntry {
  SILLocation Loc;
  const char *Action;
  ASTContext &Context;
public:
  PrettyStackTraceSILLocation(const char *action, SILLocation loc,
                              ASTContext &C)
    : Loc(loc), Action(action), Context(C) {}
  virtual void print(llvm::raw_ostream &OS) const;
};


/// Observe that we are doing some processing of a SIL function.
class PrettyStackTraceSILFunction : public llvm::PrettyStackTraceEntry {
  const SILFunction *TheFn;
  const char *Action;
public:
  PrettyStackTraceSILFunction(const char *action, const SILFunction *F)
    : TheFn(F), Action(action) {}
  virtual void print(llvm::raw_ostream &OS) const;
protected:
  void printFunctionInfo(llvm::raw_ostream &out) const;
};

/// Observe that we are visiting SIL nodes.
class PrettyStackTraceSILNode : public llvm::PrettyStackTraceEntry {
  const SILNode *Node;
  const char *Action;

public:
  PrettyStackTraceSILNode(const char *action, const SILNode *node)
    : Node(node), Action(action) {}

  virtual void print(llvm::raw_ostream &OS) const;
};

} // end namespace swift

#endif
