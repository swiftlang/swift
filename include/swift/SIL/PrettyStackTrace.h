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
#include "swift/SIL/SILNode.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/PrettyStackTrace.h"

namespace swift {
class ASTContext;
class SILFunction;

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
  virtual void print(llvm::raw_ostream &OS) const override;
};


/// Observe that we are doing some processing of a SIL function.
class PrettyStackTraceSILFunction : public llvm::PrettyStackTraceEntry {
  const SILFunction *func;

  /// An inline buffer of characters used if we are passed a twine.
  SmallString<256> data;

  /// This points either at a user provided const char * string or points at the
  /// inline message buffer that is initialized with data from a twine on
  /// construction.
  StringRef action;

public:
  PrettyStackTraceSILFunction(const char *action, const SILFunction *func)
      : func(func), data(), action(action) {}

  PrettyStackTraceSILFunction(llvm::Twine &&twine, const SILFunction *func)
      : func(func), data(), action(twine.toNullTerminatedStringRef(data)) {}

  virtual void print(llvm::raw_ostream &os) const override;

protected:
  void printFunctionInfo(llvm::raw_ostream &out) const;
};

/// Observe that we are visiting SIL nodes.
class PrettyStackTraceSILNode : public llvm::PrettyStackTraceEntry {
  const SILNode *Node;
  const char *Action;

public:
  PrettyStackTraceSILNode(const char *action, SILNodePointer node)
    : Node(node), Action(action) {}

  virtual void print(llvm::raw_ostream &OS) const override;
};

/// Observe that we are processing a reference to a SIL decl.
class PrettyStackTraceSILDeclRef : public llvm::PrettyStackTraceEntry {
  SILDeclRef declRef;
  StringRef action;

public:
  PrettyStackTraceSILDeclRef(const char *action, SILDeclRef declRef)
      : declRef(declRef), action(action) {}

  virtual void print(llvm::raw_ostream &os) const override;
};

} // end namespace swift

#endif
