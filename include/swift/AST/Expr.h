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

#include "swift/AST/Identifier.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/NullablePtr.h"
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
  class VarDecl;
  class AnonDecl;
  
enum ExprKind {
  IntegerLiteralKind,
  DeclRefExprKind,
  TupleExprKind,
  UnresolvedDotExprKind,
  TupleElementExprKind,
  ApplyExprKind,
  SequenceExprKind,
  BraceExprKind,
  ClosureExprKind,
  BinaryExprKind
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
  /// FIXME: QOI: Need to extend this to do full source ranges like Clang.
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
  Identifier *SubExprNames;  // Can be null if no names.
  unsigned NumSubExprs;
  llvm::SMLoc RParenLoc;
  
  TupleExpr(llvm::SMLoc lparenloc, Expr **subexprs, Identifier *subexprnames,
            unsigned numsubexprs, llvm::SMLoc rparenloc, Type *Ty)
    : Expr(TupleExprKind, Ty), LParenLoc(lparenloc), SubExprs(subexprs),
      SubExprNames(subexprnames), NumSubExprs(numsubexprs),
      RParenLoc(rparenloc) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == TupleExprKind; }
};

/// UnresolvedDotExpr - A field access to an expression with dependent type.
class UnresolvedDotExpr : public Expr {
public:
  Expr *SubExpr;
  llvm::SMLoc DotLoc;
  Identifier Name;
  llvm::SMLoc NameLoc;
  
  UnresolvedDotExpr(Expr *subexpr, llvm::SMLoc dotloc, Identifier name,
                    llvm::SMLoc nameloc, Type *Ty)
  : Expr(UnresolvedDotExprKind, Ty), SubExpr(subexpr), DotLoc(dotloc),
    Name(name), NameLoc(nameloc) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDotExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == UnresolvedDotExprKind;}
};

/// TupleElementExpr - Refer to an element of a tuple, e.g. "(1,2).field0".
class TupleElementExpr : public Expr {
public:
  Expr *SubExpr;
  llvm::SMLoc DotLoc;
  unsigned FieldNo;
  llvm::SMLoc NameLoc;
  
  TupleElementExpr(Expr *subexpr, llvm::SMLoc dotloc, unsigned fieldno,
                   llvm::SMLoc nameloc, Type *Ty)
  : Expr(TupleElementExprKind, Ty), SubExpr(subexpr), DotLoc(dotloc),
    FieldNo(fieldno), NameLoc(nameloc) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleElementExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == TupleElementExprKind; }
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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ApplyExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ApplyExprKind; }
};

/// SequenceExpr - a series of expressions which should be evaluated
/// sequentially, e.g. foo()  bar().  This is like BraceExpr but doesn't have
/// semicolons, braces, or declarations and can never be empty.
class SequenceExpr : public Expr {
public:
  Expr **Elements;
  unsigned NumElements;
  
  SequenceExpr(Expr **elements, unsigned numElements, Type *Ty)
    : Expr(SequenceExprKind, Ty), Elements(elements), NumElements(numElements) {
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SequenceExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == SequenceExprKind; }
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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BraceExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == BraceExprKind; }
};

  
/// ClosureExpr - An expression which is implicitly created by using an
/// expression in a function context where the expression's type matches the
/// result of the function.  The Decl list indicates which decls the formal
/// arguments are bound to.
class ClosureExpr : public Expr {
public:
  Expr *Input;
  
  /// AnonArgList - This specifies the decls that named anonymous arguments are
  /// bound by this closure.  Note that elements of this list may be null when
  /// the argument is not bound to an argument and that this list may be null if
  /// named anonymous arguments are not used.  If the ArgList pointer is
  /// non-null, then its length is indicated by getNumArgs().
  llvm::NullablePtr<AnonDecl> *ArgList;
  
  ClosureExpr(Expr *input, llvm::NullablePtr<AnonDecl> *arglist, Type *ResultTy)
    : Expr(ClosureExprKind, ResultTy), Input(input), ArgList(arglist) {}

  /// getNumArgs - Return the number of arguments that this closure expr takes.
  /// This is the length of the ArgList.
  unsigned getNumArgs() const;
  
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClosureExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ClosureExprKind; }
};
  
/// BinaryExpr - Infix binary expressions like 'x+y'.
class BinaryExpr : public Expr {
public:
  Expr *LHS;
  NamedDecl *Fn;
  llvm::SMLoc OpLoc;
  Expr *RHS;
  
  BinaryExpr(Expr *lhs, NamedDecl *fn, llvm::SMLoc oploc, Expr *rhs, Type *Ty)
    : Expr(BinaryExprKind, Ty), LHS(lhs), Fn(fn), OpLoc(oploc), RHS(rhs) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BinaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == BinaryExprKind; }
};
  
} // end namespace swift

#endif
