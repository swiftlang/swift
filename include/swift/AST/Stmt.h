//===--- Stmt.h - Swift Language Statement ASTs -----------------*- C++ -*-===//
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
// This file defines the Stmt class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_STMT_H
#define SWIFT_AST_STMT_H

#include "swift/AST/LLVM.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}

namespace swift {
  class ASTContext;
  class Decl;
  class Expr;

enum class StmtKind {
  Semi,
  Brace,
  If
};

/// Stmt - Base class for all statements in swift.
class Stmt {
  Stmt(const Stmt&) = delete;
  void operator=(const Stmt&) = delete;
public:
  /// Kind - The subclass of Stmt that this is.
  const StmtKind Kind;
  
  Stmt(StmtKind kind) : Kind(kind) {}

  /// getLocStart - Return the location of the start of the expression.
  /// FIXME: QOI: Need to extend this to do full source ranges like Clang.
  SMLoc getLocStart() const;
  
  void dump() const;
  void print(raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *) { return true; }

  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();
  
  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;

};

/// SemiStmt - A semicolon, the noop statement: ";"
class SemiStmt : public Stmt {
public:
  SMLoc Loc;
  
  SemiStmt(SMLoc Loc) : Stmt(StmtKind::Semi), Loc(Loc) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SemiStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->Kind == StmtKind::Semi; }
};


/// BraceStmt - A brace enclosed sequence of expressions, stmts, or decls, like
/// { 4; 5 }.
class BraceStmt : public Stmt {
public:
  SMLoc LBLoc;
  
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;
  // FIXME: Switch to MutableArrayRef.
  ExprStmtOrDecl *Elements;
  unsigned NumElements;
  
  SMLoc RBLoc;
  
  BraceStmt(SMLoc lbloc, ExprStmtOrDecl *elements,
            unsigned numelements, SMLoc rbloc)
  : Stmt(StmtKind::Brace), LBLoc(lbloc), Elements(elements),
    NumElements(numelements), RBLoc(rbloc) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const BraceStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->Kind == StmtKind::Brace; }
};


/// IfStmt - if/then/else statement.  If no 'else' is specified, then the
/// ElseLoc location is not specified and the Else statement is null.  The
/// condition of the 'if' is required to have a __builtin_int1 type.
class IfStmt : public Stmt {
public:
  SMLoc IfLoc;
  Expr *Cond;
  Stmt *Then;
  SMLoc ElseLoc;
  Stmt *Else;
  
  IfStmt(SMLoc IfLoc, Expr *cond, Stmt *Then, SMLoc ElseLoc,
         Stmt *Else)
  : Stmt(StmtKind::If),
    IfLoc(IfLoc), Cond(cond), Then(Then), ElseLoc(ElseLoc), Else(Else) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IfStmt *) { return true; }
  static bool classof(const Stmt *S) { return S->Kind == StmtKind::If; }
};

  
} // end namespace swift

#endif
