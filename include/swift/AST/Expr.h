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
  class ValueDecl;
  class Decl;
  
enum ExprKind {
  IntegerLiteralKind,
  DeclRefExprKind,
  UnresolvedDeclRefExprKind,
  UnresolvedMemberExprKind,
  TupleExprKind,
  UnresolvedDotExprKind,
  TupleElementExprKind,
  ApplyExprKind,
  SequenceExprKind,
  BraceExprKind,
  ClosureExprKind,
  AnonClosureArgExprKind,
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
  
  Expr(ExprKind kind, Type *ty = 0) : Kind(kind), Ty(ty) {}

  /// getLocStart - Return the location of the start of the expression.
  /// FIXME: QOI: Need to extend this to do full source ranges like Clang.
  llvm::SMLoc getLocStart() const;
  
  
  enum WalkOrder {
    Walk_PreOrder,
    Walk_PostOrder
  };
  
  /// WalkExpr - This function walks all the subexpressions under this
  /// expression and invokes the specified function pointer on them.  The
  /// function pointer is invoked both before and after the children are visted,
  /// the WalkOrder specifies at each invocation which stage it is.  If the
  /// function pointer returns NULL then the walk is terminated and WalkExpr
  /// returns NULL.  Otherwise, the returned expression is spliced back into the
  /// AST or returned from WalkExpr if at the top-level.
  /// 
  Expr *WalkExpr(Expr *(*Fn)(Expr *E, WalkOrder Order, void *Data), void *Data);
  
  
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
  llvm::StringRef Val;  // Use StringRef instead of APInt, APInt leaks.
  llvm::SMLoc Loc;
  
  IntegerLiteral(llvm::StringRef V, llvm::SMLoc L)
    : Expr(IntegerLiteralKind), Val(V), Loc(L) {}
  
  uint64_t getValue() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IntegerLiteral *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == IntegerLiteralKind; }
};

/// DeclRefExpr - A reference to a value, "x".
class DeclRefExpr : public Expr {
public:
  ValueDecl *D;
  llvm::SMLoc Loc;
  
  DeclRefExpr(ValueDecl *d, llvm::SMLoc L, Type *Ty = 0)
    : Expr(DeclRefExprKind, Ty), D(d), Loc(L) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DeclRefExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == DeclRefExprKind; }
};
  
/// UnresolvedDeclRefExpr - This represents use of an undeclared identifier,
/// which may ultimately be a use of something that hasn't been defined yet, it
/// may be a use of something that got imported (which will be resolved during
/// sema), or may just be a use of an unknown identifier.
///
class UnresolvedDeclRefExpr : public Expr {
public:
  Identifier Name;
  llvm::SMLoc Loc;
  
  UnresolvedDeclRefExpr(Identifier name, llvm::SMLoc loc)
    : Expr(UnresolvedDeclRefExprKind), Name(name), Loc(loc) {
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDeclRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == UnresolvedDeclRefExprKind;
  }
};

/// UnresolvedMemberExpr - This represents ':foo', an unresolved reference to a
/// member, which is to be resolved with context sensitive type information into
/// bar::foo.  These always have dependent type.
class UnresolvedMemberExpr : public Expr {
public:
  llvm::SMLoc ColonLoc;
  llvm::SMLoc NameLoc;
  Identifier Name;
  
  UnresolvedMemberExpr(llvm::SMLoc colonLoc, llvm::SMLoc nameLoc,
                       Identifier name)
    : Expr(UnresolvedMemberExprKind),
      ColonLoc(colonLoc), NameLoc(nameLoc), Name(name) {
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedMemberExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == UnresolvedMemberExprKind;
  }
};
  
/// TupleExpr - Parenthesized expressions like '(x+x)' and '(x, y, 4)'.  Tuple
/// types automatically decay if they have a single element, this means that
/// single element tuple literals, such as "(4)", will exist in the AST, but
/// have a result type that is the same as the input operand type.
class TupleExpr : public Expr {
public:
  llvm::SMLoc LParenLoc;
  /// SubExprs - Elements of these can be set to null to get the default init
  /// value for the tuple element.
  Expr **SubExprs;
  Identifier *SubExprNames;  // Can be null if no names.
  unsigned NumSubExprs;
  llvm::SMLoc RParenLoc;
  
  TupleExpr(llvm::SMLoc lparenloc, Expr **subexprs, Identifier *subexprnames,
            unsigned numsubexprs, llvm::SMLoc rparenloc, Type *Ty = 0)
    : Expr(TupleExprKind, Ty), LParenLoc(lparenloc), SubExprs(subexprs),
      SubExprNames(subexprnames), NumSubExprs(numsubexprs),
      RParenLoc(rparenloc) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == TupleExprKind; }
};

/// UnresolvedDotExpr - A field access (foo.bar) on an expression with dependent
/// type.
class UnresolvedDotExpr : public Expr {
public:
  Expr *SubExpr;
  llvm::SMLoc DotLoc;
  Identifier Name;
  llvm::SMLoc NameLoc;
  
  UnresolvedDotExpr(Expr *subexpr, llvm::SMLoc dotloc, Identifier name,
                    llvm::SMLoc nameloc)
  : Expr(UnresolvedDotExprKind), SubExpr(subexpr), DotLoc(dotloc),
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
                   llvm::SMLoc nameloc, Type *ty = 0)
  : Expr(TupleElementExprKind, ty), SubExpr(subexpr), DotLoc(dotloc),
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
  
  SequenceExpr(Expr **elements, unsigned numElements)
    : Expr(SequenceExprKind), Elements(elements), NumElements(numElements) {
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
  
  llvm::PointerUnion<Expr*, Decl*> *Elements;
  unsigned NumElements;
  
  /// This is true if the last expression in the brace expression is missing a
  /// semicolon after it.
  bool MissingSemi;
  llvm::SMLoc RBLoc;

  BraceExpr(llvm::SMLoc lbloc, llvm::PointerUnion<Expr*, Decl*> *elements,
            unsigned numelements, bool missingsemi, llvm::SMLoc rbloc)
    : Expr(BraceExprKind), LBLoc(lbloc), Elements(elements),
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
  
  ClosureExpr(Expr *input, Type *ResultTy)
    : Expr(ClosureExprKind, ResultTy), Input(input) {}

  /// getNumArgs - Return the number of arguments that this closure expr takes.
  unsigned getNumArgs() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClosureExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ClosureExprKind; }
};
  
class AnonClosureArgExpr : public Expr {
public:
  unsigned ArgNo;
  llvm::SMLoc Loc;
  
  AnonClosureArgExpr(unsigned argNo, llvm::SMLoc loc)
    : Expr(AnonClosureArgExprKind), ArgNo(argNo), Loc(loc) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const AnonClosureArgExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind ==AnonClosureArgExprKind;}
};
  
/// BinaryExpr - Infix binary expressions like 'x+y'.
class BinaryExpr : public Expr {
public:
  Expr *LHS;
  ValueDecl *Fn;
  llvm::SMLoc OpLoc;
  Expr *RHS;
  
  BinaryExpr(Expr *lhs, ValueDecl *fn, llvm::SMLoc oploc, Expr *rhs, Type *Ty=0)
    : Expr(BinaryExprKind, Ty), LHS(lhs), Fn(fn), OpLoc(oploc), RHS(rhs) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BinaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == BinaryExprKind; }
};
  
} // end namespace swift

#endif
