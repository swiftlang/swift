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
  class ASTContext;
  class Decl;
  class Expr;
  class Stmt;

/// PrettyStackTraceDecl - Observe that we are processing a specific
/// declaration.
class PrettyStackTraceDecl : public llvm::PrettyStackTraceEntry {
  Decl *TheDecl;
  const char *Action;
public:
  PrettyStackTraceDecl(const char *action, Decl *D)
    : TheDecl(D), Action(action) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

/// PrettyStackTraceExpr - Observe that we are processing a specific
/// expression.
class PrettyStackTraceExpr : public llvm::PrettyStackTraceEntry {
  ASTContext &Context;
  Expr *TheExpr;
  const char *Action;
public:
  PrettyStackTraceExpr(ASTContext &C, const char *action, Expr *E)
    : Context(C), TheExpr(E), Action(action) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

/// PrettyStackTraceStmt - Observe that we are processing a specific
/// statement.
class PrettyStackTraceStmt : public llvm::PrettyStackTraceEntry {
  ASTContext &Context;
  Stmt *TheStmt;
  const char *Action;
public:
  PrettyStackTraceStmt(ASTContext &C, const char *action, Stmt *S)
    : Context(C), TheStmt(S), Action(action) {}
  virtual void print(llvm::raw_ostream &OS) const;
};

} // end namespace swift

#endif
