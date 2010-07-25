//===--- Expr.h - Swift Language Expression ASTs ----------------*- C++ -*-===//
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
// This file defines the Expr class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_EXPR_H
#define SWIFT_AST_EXPR_H

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class raw_ostream;
  template <typename PT1, typename PT2>
  class PointerUnion;
}

namespace swift {
  class ASTContext;
  class Type;
  class NamedDecl;
  
enum ExprKind {
  IntegerLiteralKind,
  DeclRefExprKind,
  TupleExprKind,
  ApplyExprKind,
  BraceExprKind,
  
  BinaryAddExprKind,
  BinarySubExprKind,
  BinaryMulExprKind,
  BinaryDivExprKind,
  
  
  First_BinaryExpr = BinaryAddExprKind,
  Last_BinaryExpr = BinaryDivExprKind
};
  
  
/// Expr - Base class for all expressions in swift.
class Expr {
  Expr(const Expr&);                 // DO NOT IMPLEMENT
  void operator=(const Expr&);       // DO NOT IMPLEMENT
public:
  /// Kind - The subclass of Expr that this is.
  const ExprKind Kind;

  /// Ty - This is the type of the expression.
  Type *Ty;
  
  Expr(ExprKind kind, Type *ty) : Kind(kind), Ty(ty) {}

  /// getLocStart - Return the location of the start of the expression.
  /// FIXME: Need to extend this to do full source ranges like Clang.
  llvm::SMLoc getLocStart() const;
  
  
  void dump() const;
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Expr *) { return true; }

private:
  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw();  // DO NOT IMPLEMENT.
  void operator delete(void *Data) throw();  // DO NOT IMPLEMENT.
  void *operator new(size_t Bytes, void *Mem) throw();  // DO NOT IMPLEMENT.
public:
  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};


/// IntegerLiteral - Integer literal, like '4'.
class IntegerLiteral : public Expr {
public:
  llvm::StringRef Val;  // TODO: uint64_t.  APInt leaks.
  llvm::SMLoc Loc;
  
  IntegerLiteral(llvm::StringRef V, llvm::SMLoc L, Type *Ty)
    : Expr(IntegerLiteralKind, Ty), Val(V), Loc(L) {}
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const IntegerLiteral *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == IntegerLiteralKind; }
};

/// DeclRefExpr - A reference to a variable, "x".
class DeclRefExpr : public Expr {
public:
  NamedDecl *D;
  llvm::SMLoc Loc;
  
  DeclRefExpr(NamedDecl *d, llvm::SMLoc L, Type *Ty)
    : Expr(DeclRefExprKind, Ty), D(d), Loc(L) {}
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DeclRefExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == DeclRefExprKind; }
};
  
  
/// TupleExpr - Parenthesized expressions like '(x+x)' and '(x, y, 4)'.  Tuple
/// types automatically decay if they have a single element, this means that
/// single element tuple literals, such as "(4)", will exist in the AST, but
/// have a result type that is the same as the input operand type.
class TupleExpr : public Expr {
public:
  llvm::SMLoc LParenLoc;
  Expr **SubExprs;
  unsigned NumSubExprs;
  llvm::SMLoc RParenLoc;
  
  TupleExpr(llvm::SMLoc lparenloc, Expr **subexprs, unsigned numsubexprs,
            llvm::SMLoc rparenloc, Type *Ty)
    : Expr(TupleExprKind, Ty), LParenLoc(lparenloc), SubExprs(subexprs),
      NumSubExprs(numsubexprs), RParenLoc(rparenloc) {}

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == TupleExprKind; }
};

/// ApplyExpr - Application of an argument to a function, which occurs
/// syntactically through juxtaposition.  For example, f(1,2) is parsed as
/// 'f' '(1,2)' which applies a tuple to the function, producing a result.
class ApplyExpr : public Expr {
public:
  /// Fn - The function being invoked.
  Expr *Fn;
  /// Argument - The one argument being passed to it.
  Expr *Arg;
  ApplyExpr(Expr *fn, Expr *arg, Type *Ty)
    : Expr(ApplyExprKind, Ty), Fn(fn), Arg(arg) {}

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ApplyExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ApplyExprKind; }
};
  
/// BraceExpr - A brace enclosed sequence of expressions, like { 4; 5 }.  If the
/// final expression is terminated with a ;, the result type of the brace expr
/// is void, otherwise it is the value of the last expression.
class BraceExpr : public Expr {
public:
  llvm::SMLoc LBLoc;
  
  llvm::PointerUnion<Expr*, NamedDecl*> *Elements;
  unsigned NumElements;
  
  /// This is true if the last expression in the brace expression is missing a
  /// semicolon after it.
  bool MissingSemi;
  llvm::SMLoc RBLoc;

  BraceExpr(llvm::SMLoc lbloc, llvm::PointerUnion<Expr*, NamedDecl*> *elements,
            unsigned numelements, bool missingsemi, llvm::SMLoc rbloc, Type *Ty)
    : Expr(BraceExprKind, Ty), LBLoc(lbloc), Elements(elements),
      NumElements(numelements), MissingSemi(missingsemi), RBLoc(rbloc) {}

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const BraceExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == BraceExprKind; }
};

/// BinaryExpr - Binary expressions like 'x+y'.
class BinaryExpr : public Expr {
public:
  Expr *LHS;
  llvm::SMLoc OpLoc;
  Expr *RHS;
  
  BinaryExpr(ExprKind kind, Expr *lhs, llvm::SMLoc oploc, Expr *rhs, Type *Ty)
    : Expr(kind, Ty), LHS(lhs), OpLoc(oploc), RHS(rhs) {}

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BinaryExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind >= First_BinaryExpr && E->Kind <= Last_BinaryExpr; }
};
  
} // end namespace swift

#endif
