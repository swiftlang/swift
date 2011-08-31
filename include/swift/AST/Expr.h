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

#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "swift/AST/WalkOrder.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class ASTContext;
  class Type;
  class ArgDecl;
  class ValueDecl;
  class Decl;
  class Stmt;
  class BraceStmt;
  class TypeAliasDecl;
  
enum class ExprKind {
  IntegerLiteral,
  DeclRef,
  OverloadSetRef,
  UnresolvedDeclRef,
  UnresolvedMember,
  UnresolvedScopedIdentifier,
  Tuple,
  UnresolvedDot,
  TupleElement,
  TupleShuffle,
  Sequence,
  Func,
  Closure,
  AnonClosureArg,
  Call,      First_ApplyExpr = Call,
  Unary,
  Binary,    Last_ApplyExpr = Binary
};
  
  
/// Expr - Base class for all expressions in swift.
class Expr {
  Expr(const Expr&) = delete;
  void operator=(const Expr&) = delete;
public:
  /// Kind - The subclass of Expr that this is.
  const ExprKind Kind;

  /// Ty - This is the type of the expression.
  Type Ty;
  
  Expr(ExprKind kind, Type ty = Type()) : Kind(kind), Ty(ty) {}

  /// getLocStart - Return the location of the start of the expression.
  /// FIXME: QOI: Need to extend this to do full source ranges like Clang.
  SMLoc getLocStart() const;

  /// walk - This recursively walks all of the statements and expressions
  /// contained within a statement and invokes the ExprFn and StmtFn blocks on
  /// each.
  ///
  /// The block pointers are invoked both before and after the children are
  /// visted, with the WalkOrder specifing at each invocation which stage it is.
  /// If the block pointer returns a non-NULL value, then the returned
  /// expression or statement is spliced back into the AST or returned from
  /// 'walk' if at the top-level.
  ///
  /// If block pointer returns NULL from a pre-order invocation, then the
  /// subtree is not visited.  If the block pointer returns NULL from a
  /// post-order invocation, then the walk is terminated and 'walk returns
  /// NULL.
  ///
  Expr *walk(Expr *(^ExprFn)(Expr *E, WalkOrder Order),
             Stmt *(^StmtFn)(Stmt *E, WalkOrder Order) = 0);
  
  /// ConversionRank - This enum specifies the rank of an implicit conversion
  /// of a value from one type to another.  These are ordered from cheapest to
  /// most expensive.
  enum ConversionRank {
    /// CR_Identity - It is free to convert these two types.  For example,
    /// identical types return this, types that are just aliases of each other
    /// do as well, conversion of a scalar to a single-element tuple, etc.
    CR_Identity,
    
    /// CR_AutoClosure - Conversion of the source type to the destination type
    /// requires the introduction of a closure.  This occurs with a conversion
    /// from "()" to "()->()" type, for example.
    CR_AutoClosure,
    
    /// CR_Invalid - It isn't valid to convert these types.  For example, it
    /// isn't valid to convert a value of type "()" to "(int)".
    CR_Invalid
  };
  
  /// getRankOfConversionTo - Return the rank of a conversion from the current
  /// type to the specified type.
  ConversionRank getRankOfConversionTo(Type DestTy, ASTContext &Ctx) const;
  
  void dump() const;
  void print(raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Expr *) { return true; }

  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  

  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
};


/// IntegerLiteralExpr - Integer literal, like '4'.
class IntegerLiteralExpr : public Expr {
public:
  StringRef Val;  // Use StringRef instead of APInt, APInt leaks.
  SMLoc Loc;
  
  IntegerLiteralExpr(StringRef V, SMLoc L, Type Ty)
    : Expr(ExprKind::IntegerLiteral, Ty), Val(V), Loc(L) {}
  
  uint64_t getValue() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IntegerLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::IntegerLiteral;
  }
};

/// DeclRefExpr - A reference to a value, "x".
class DeclRefExpr : public Expr {
public:
  ValueDecl *D;
  SMLoc Loc;
  
  DeclRefExpr(ValueDecl *d, SMLoc L, Type Ty = Type())
    : Expr(ExprKind::DeclRef, Ty), D(d), Loc(L) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DeclRefExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::DeclRef; }
};

/// OverloadSetRefExpr - A reference to an overloaded set of values with a
/// single name.
class OverloadSetRefExpr : public Expr {
public:
  ArrayRef<ValueDecl*> Decls;
  SMLoc Loc;
  
  OverloadSetRefExpr(ArrayRef<ValueDecl*> decls, SMLoc L,
                     Type Ty = Type())
  : Expr(ExprKind::OverloadSetRef, Ty), Decls(decls), Loc(L) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OverloadSetRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::OverloadSetRef;
  }
};
  
/// UnresolvedDeclRefExpr - This represents use of an undeclared identifier,
/// which may ultimately be a use of something that hasn't been defined yet, it
/// may be a use of something that got imported (which will be resolved during
/// sema), or may just be a use of an unknown identifier.
///
class UnresolvedDeclRefExpr : public Expr {
public:
  Identifier Name;
  SMLoc Loc;
  
  UnresolvedDeclRefExpr(Identifier name, SMLoc loc)
    : Expr(ExprKind::UnresolvedDeclRef), Name(name), Loc(loc) {
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDeclRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::UnresolvedDeclRef;
  }
};

/// UnresolvedMemberExpr - This represents ':foo', an unresolved reference to a
/// member, which is to be resolved with context sensitive type information into
/// bar::foo.  These always have dependent type.
class UnresolvedMemberExpr : public Expr {
public:
  SMLoc ColonLoc;
  SMLoc NameLoc;
  Identifier Name;
  
  UnresolvedMemberExpr(SMLoc colonLoc, SMLoc nameLoc,
                       Identifier name)
    : Expr(ExprKind::UnresolvedMember),
      ColonLoc(colonLoc), NameLoc(nameLoc), Name(name) {
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedMemberExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::UnresolvedMember;
  }
};
  
/// UnresolvedScopedIdentifierExpr - This represents "foo::bar", an unresolved
/// reference to a type foo and a member bar within it.
class UnresolvedScopedIdentifierExpr : public Expr {
public:
  TypeAliasDecl *TypeDecl;
  SMLoc TypeDeclLoc, ColonColonLoc, NameLoc;
  Identifier Name;
  
  UnresolvedScopedIdentifierExpr(TypeAliasDecl *typeDecl,
                                 SMLoc typeDeclLoc, SMLoc colonLoc,
                                 SMLoc nameLoc, Identifier name)
  : Expr(ExprKind::UnresolvedScopedIdentifier), TypeDecl(typeDecl),
    TypeDeclLoc(typeDeclLoc), ColonColonLoc(colonLoc), NameLoc(nameLoc),
    Name(name) {
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedScopedIdentifierExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::UnresolvedScopedIdentifier;
  }
};
  
/// TupleExpr - Parenthesized expressions like '(x+x)' and '(x, y, 4)'.  Tuple
/// types automatically decay if they have a single element, this means that
/// single element tuple literals, such as "(4)", will exist in the AST, but
/// have a result type that is the same as the input operand type.
///
/// When a tuple element is formed with a default value for the type, the
/// corresponding SubExpr element will be null.
class TupleExpr : public Expr {
public:
  SMLoc LParenLoc;
  /// SubExprs - Elements of these can be set to null to get the default init
  /// value for the tuple element.
  // FIXME: Switch to MutableArrayRef.
  Expr **SubExprs;
  Identifier *SubExprNames;  // Can be null if no names.
  unsigned NumSubExprs;
  SMLoc RParenLoc;
  
  /// IsGrouping - True if this is a syntactic grouping expression where the
  /// source and result types are the same.  This is only true for
  /// single-element tuples with no element name.
  bool IsGrouping;
  
  TupleExpr(SMLoc lparenloc, Expr **subexprs, Identifier *subexprnames,
            unsigned numsubexprs, SMLoc rparenloc, bool isGrouping,
            Type Ty = Type())
    : Expr(ExprKind::Tuple, Ty), LParenLoc(lparenloc), SubExprs(subexprs),
      SubExprNames(subexprnames), NumSubExprs(numsubexprs),
      RParenLoc(rparenloc), IsGrouping(isGrouping) {
    assert((!isGrouping ||
            (NumSubExprs == 1 && getElementName(0).empty() && SubExprs[0])) &&
           "Invalid grouping paren");
  }

  Identifier getElementName(unsigned i) const {
    assert(i < NumSubExprs && "Invalid element index");
    return SubExprNames ? SubExprNames[i] : Identifier();
  }
  
  /// isGroupingParen - Return true if this is a grouping parenthesis, in which
  /// the input and result types are the same.
  bool isGroupingParen() const {
    return IsGrouping;
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Tuple; }
};

/// UnresolvedDotExpr - A field access (foo.bar) on an expression with dependent
/// type.
class UnresolvedDotExpr : public Expr {
public:
  Expr *SubExpr;
  SMLoc DotLoc;
  Identifier Name;
  SMLoc NameLoc;
  
  /// ResolvedDecl - If the name refers to any local or top-level declarations,
  /// the name binder fills them in here.
  ArrayRef<ValueDecl*> ResolvedDecls;
  
  UnresolvedDotExpr(Expr *subexpr, SMLoc dotloc, Identifier name,
                    SMLoc nameloc)
  : Expr(ExprKind::UnresolvedDot), SubExpr(subexpr), DotLoc(dotloc),
    Name(name), NameLoc(nameloc) {}
  
  SMLoc getLocStart() const {
    return SubExpr ? SubExpr->getLocStart() : DotLoc;
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDotExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::UnresolvedDot;
  }
};

/// TupleElementExpr - Refer to an element of a tuple, e.g. "(1,2).field0".
class TupleElementExpr : public Expr {
public:
  Expr *SubExpr;
  SMLoc DotLoc;
  unsigned FieldNo;
  SMLoc NameLoc;
  
  TupleElementExpr(Expr *subexpr, SMLoc dotloc, unsigned fieldno,
                   SMLoc nameloc, Type ty = Type())
  : Expr(ExprKind::TupleElement, ty), SubExpr(subexpr), DotLoc(dotloc),
    FieldNo(fieldno), NameLoc(nameloc) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleElementExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::TupleElement;
  }
};

/// TupleShuffleExpr - This represents a permutation of a tuple value to a new
/// tuple type.  The expression's type is known to be a tuple type and the
/// subexpression is known to have a tuple type as well.
class TupleShuffleExpr : public Expr {
public:
  Expr *SubExpr;
  
  /// This contains an entry for each element in the Expr type.  Each element
  /// specifies which index from the SubExpr that the destination element gets.
  /// If the element value is -1, then the destination value gets the default
  /// initializer for that tuple element value.
  ArrayRef<int> ElementMapping;
  
  TupleShuffleExpr(Expr *subExpr, ArrayRef<int> elementMapping, Type Ty)
    : Expr(ExprKind::TupleShuffle, Ty), SubExpr(subExpr),
      ElementMapping(elementMapping) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleShuffleExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::TupleShuffle;
  }
};
  
/// SequenceExpr - a series of expressions which should be evaluated
/// sequentially, e.g. foo()  bar().
class SequenceExpr : public Expr {
public:
  // FIXME: Switch to MutableArrayRef.
  Expr **Elements;
  unsigned NumElements;
  
  SequenceExpr(Expr **elements, unsigned numElements)
    : Expr(ExprKind::Sequence), Elements(elements), NumElements(numElements) {
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SequenceExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Sequence; }
};

/// FuncExpr - An explicit unnamed func definition, which can optionally
/// have named arguments.
///    e.g.  func(a : int) -> int { return a+1 }
class FuncExpr : public Expr, public DeclContext {
public:
  SMLoc FuncLoc;
  
  ArrayRef<ArgDecl*> NamedArgs;
  BraceStmt *Body;
  
  FuncExpr(SMLoc FuncLoc, Type FnType, ArrayRef<ArgDecl*> NamedArgs, 
           BraceStmt *Body, DeclContext *Parent)
    : Expr(ExprKind::Func, FnType),
      DeclContext(DeclContextKind::FuncExpr, Parent),
      FuncLoc(FuncLoc), NamedArgs(NamedArgs), Body(Body) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FuncExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Func; }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::FuncExpr;
  }

};
  
/// ClosureExpr - An expression which is implicitly created by using an
/// expression in a function context where the expression's type matches the
/// result of the function.  The Decl list indicates which decls the formal
/// arguments are bound to.
class ClosureExpr : public Expr {
public:
  Expr *Input;
  
  ClosureExpr(Expr *input, Type ResultTy)
    : Expr(ExprKind::Closure, ResultTy), Input(input) {}

  /// getNumArgs - Return the number of arguments that this closure expr takes.
  unsigned getNumArgs() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClosureExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Closure; }
};
  
class AnonClosureArgExpr : public Expr {
public:
  unsigned ArgNo;
  SMLoc Loc;
  
  AnonClosureArgExpr(unsigned argNo, SMLoc loc)
    : Expr(ExprKind::AnonClosureArg), ArgNo(argNo), Loc(loc) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const AnonClosureArgExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind == ExprKind::AnonClosureArg;
  }
};
  
/// ApplyExpr - Superclass of various function calls, which apply an argument to
/// a function to get a result.
class ApplyExpr : public Expr {
public:
  /// Fn - The function being called.
  Expr *Fn;

  /// Argument - The one argument being passed to it.
  Expr *Arg;

  ApplyExpr(ExprKind Kind, Expr *Fn, Expr *Arg, Type Ty = Type())
    : Expr(Kind, Ty), Fn(Fn), Arg(Arg) {
    assert(classof((Expr*)this) && "ApplyExpr::classof out of date");
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ApplyExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->Kind >= ExprKind::First_ApplyExpr &&
           E->Kind <= ExprKind::Last_ApplyExpr;
  }
};
  
/// CallExpr - Application of an argument to a function, which occurs
/// syntactically through juxtaposition with a TupleExpr whose
/// leading '(' is unspaced.
class CallExpr : public ApplyExpr {
public:
  CallExpr(Expr *Fn, Expr *Arg, Type Ty)
    : ApplyExpr(ExprKind::Call, Fn, Arg, Ty) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const CallExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Call; }
};
  
/// UnaryExpr - Prefix unary expressions like '!y'.
class UnaryExpr : public ApplyExpr {
public:
  UnaryExpr(Expr *Fn, Expr *Arg, Type Ty = Type())
    : ApplyExpr(ExprKind::Unary, Fn, Arg, Ty) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Unary; }
};
  
/// BinaryExpr - Infix binary expressions like 'x+y'.  The argument is always
/// an implicit tuple expression of the type expected by the function.
class BinaryExpr : public ApplyExpr {
public:
  BinaryExpr(Expr *Fn, TupleExpr *Arg, Type Ty = Type())
    : ApplyExpr(ExprKind::Binary, Fn, Arg, Ty) {}

  /// getArgTuple - The argument is always a tuple literal.  This accessor
  /// reinterprets it properly.
  TupleExpr *getArgTuple() {
    return cast<TupleExpr>(Arg);
  }
  const TupleExpr *getArgTuple() const {
    return cast<TupleExpr>(Arg);
  }
                  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const BinaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->Kind == ExprKind::Binary; }
};
  
} // end namespace swift

#endif
