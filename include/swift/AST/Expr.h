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
#include "swift/AST/Walk.h"
#include "swift/Basic/SourceLoc.h"
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

enum class ExprKind : uint8_t {
#define EXPR(Id, Parent) Id,
#define EXPR_RANGE(Id, FirstId, LastId) \
  First_##Id##Expr = FirstId, Last_##Id##Expr = LastId,
#include "swift/AST/ExprNodes.def"
};
  
/// Expr - Base class for all expressions in swift.
class Expr {
  Expr(const Expr&) = delete;
  void operator=(const Expr&) = delete;

  /// Kind - The subclass of Expr that this is.
  const ExprKind Kind;

  /// Ty - This is the type of the expression.
  TypeJudgement Ty;

public:
  Expr(ExprKind kind, TypeJudgement ty = TypeJudgement())
    : Kind(kind), Ty(ty) {}

  /// getKind - Return the kind of this expression.
  ExprKind getKind() const { return Kind; }

  /// getValueKind - Return the value kind of this expression.
  ValueKind getValueKind() const { return Ty.getValueKind(); }

  /// getType - Return the type of this expression.
  Type getType() const { return Ty.getType(); }

  /// getTypeJudgement - Returns the full type judgement of this expression.
  TypeJudgement getTypeJudgement() const { return Ty; }

  /// setType - Sets the type of this expression.
  void setType(TypeJudgement T) { Ty = T; }

  /// setType - Sets the type of this expression.
  void setType(Type T, ValueKind VK) { setType(TypeJudgement(T, VK)); }

  /// setDependentType - Sets this expression to have the given
  /// dependent type.  This is just like setType except more
  /// self-documenting.
  void setDependentType(Type T) { setType(T, ValueKind::RValue); }

  /// \brief Return the source range of the expression.
  SourceRange getSourceRange() const;
  
  /// getStartLoc - Return the location of the start of the expression.
  SourceLoc getStartLoc() const { return getSourceRange().Start; }

  /// \brief Retrieve the location of the end of the expression.
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  
  /// getLoc - Return the caret location of this expression.
  SourceLoc getLoc() const;

  /// walk - This recursively walks all of the statements and expressions
  /// contained within an expression and invokes the ExprFn and StmtFn blocks on
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
  Expr *walk(WalkExprType ^ExprFn, WalkStmtType ^StmtFn = 0);
  
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
  ConversionRank getRankOfConversionTo(Type DestTy) const;

  void dump() const;
  void print(raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Expr *) { return true; }

  enum { Alignment = 8U };

  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = Expr::Alignment) throw();  

  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
};


/// IntegerLiteralExpr - Integer literal, like '4'.
class IntegerLiteralExpr : public Expr {
  StringRef Val;  // Use StringRef instead of APInt, APInt leaks.
  SourceLoc Loc;

public:
  IntegerLiteralExpr(StringRef Val, SourceLoc Loc)
    : Expr(ExprKind::IntegerLiteral), Val(Val), Loc(Loc) {}
  
  uint64_t getValue() const;

  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IntegerLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::IntegerLiteral;
  }
};

/// FloatLiteralExpr - Floating point literal, like '4.0'.
class FloatLiteralExpr : public Expr {
  // FIXME: Right now all floating point literals are doubles.  We should
  // generalize this when we support float.
  double Val;
  SourceLoc Loc;

public:
  FloatLiteralExpr(double Val, SourceLoc Loc, Type Ty)
    : Expr(ExprKind::FloatLiteral, TypeJudgement(Ty, ValueKind::RValue)),
      Val(Val), Loc(Loc) {}

  double getValue() const { return Val; }

  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FloatLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::FloatLiteral;
  }
};

/// DeclRefExpr - A reference to a value, "x".
class DeclRefExpr : public Expr {
  ValueDecl *D;
  SourceLoc Loc;

public:
  DeclRefExpr(ValueDecl *D, SourceLoc Loc, TypeJudgement Ty = TypeJudgement())
    : Expr(ExprKind::DeclRef, Ty), D(D), Loc(Loc) {}

  ValueDecl *getDecl() const { return D; }

  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DeclRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DeclRef;
  }
};

/// OverloadSetRefExpr - A reference to an overloaded set of values with a
/// single name.
class OverloadSetRefExpr : public Expr {
  ArrayRef<ValueDecl*> Decls;
  SourceLoc Loc;

public:
  OverloadSetRefExpr(ArrayRef<ValueDecl*> decls, SourceLoc L)
  : Expr(ExprKind::OverloadSetRef), Decls(decls), Loc(L) {}

  ArrayRef<ValueDecl*> getDecls() const { return Decls; }

  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OverloadSetRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::OverloadSetRef;
  }
};
  
/// UnresolvedDeclRefExpr - This represents use of an undeclared identifier,
/// which may ultimately be a use of something that hasn't been defined yet, it
/// may be a use of something that got imported (which will be resolved during
/// sema), or may just be a use of an unknown identifier.
///
class UnresolvedDeclRefExpr : public Expr {
  Identifier Name;
  SourceLoc Loc;

public:
  UnresolvedDeclRefExpr(Identifier name, SourceLoc loc)
    : Expr(ExprKind::UnresolvedDeclRef), Name(name), Loc(loc) {
  }
  
  Identifier getName() const { return Name; }

  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDeclRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::UnresolvedDeclRef;
  }
};

/// UnresolvedMemberExpr - This represents ':foo', an unresolved reference to a
/// member, which is to be resolved with context sensitive type information into
/// bar::foo.  These always have dependent type.
class UnresolvedMemberExpr : public Expr {
  SourceLoc ColonLoc;
  SourceLoc NameLoc;
  Identifier Name;

public:  
  UnresolvedMemberExpr(SourceLoc colonLoc, SourceLoc nameLoc,
                       Identifier name)
    : Expr(ExprKind::UnresolvedMember),
      ColonLoc(colonLoc), NameLoc(nameLoc), Name(name) {
  }

  Identifier getName() const { return Name; }
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getColonLoc() const { return ColonLoc; }

  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const { 
    return SourceRange(ColonLoc, NameLoc); 
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedMemberExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::UnresolvedMember;
  }
};
  
/// UnresolvedScopedIdentifierExpr - This represents "foo::bar", an unresolved
/// reference to a type foo and a member bar within it.
class UnresolvedScopedIdentifierExpr : public Expr {
  TypeAliasDecl *BaseTypeFromScope;
  Identifier BaseName;
  Identifier Name;
  SourceLoc BaseNameLoc, ColonColonLoc, NameLoc;
  
public:
  UnresolvedScopedIdentifierExpr(TypeAliasDecl *baseTypeFromScope,
                                 Identifier baseName, SourceLoc baseNameLoc,
                                 SourceLoc colonLoc,
                                 Identifier name, SourceLoc nameLoc)
  : Expr(ExprKind::UnresolvedScopedIdentifier),
    BaseTypeFromScope(baseTypeFromScope), BaseName(baseName), Name(name),
    BaseNameLoc(baseNameLoc), ColonColonLoc(colonLoc), NameLoc(nameLoc) {
  }

  TypeAliasDecl *getBaseTypeFromScope() const { return BaseTypeFromScope; }
  Identifier getBaseName() const { return BaseName; }
  Identifier getName() const { return Name; }
  SourceLoc getBaseNameLoc() const { return BaseNameLoc; }
  SourceLoc getColonColonLoc() const { return ColonColonLoc; }
  SourceLoc getNameLoc() const { return NameLoc; }

  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const { 
    return SourceRange(BaseNameLoc, NameLoc); 
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedScopedIdentifierExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::UnresolvedScopedIdentifier;
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
  SourceLoc LParenLoc;
  /// SubExprs - Elements of these can be set to null to get the default init
  /// value for the tuple element.
  // FIXME: Switch to MutableArrayRef.
  Expr **SubExprs;
  Identifier *SubExprNames;  // Can be null if no names.
  unsigned NumSubExprs;
  SourceLoc RParenLoc;
  
  /// IsGrouping - True if this is a syntactic grouping expression where the
  /// source and result types are the same.  This is only true for
  /// single-element tuples with no element name.
  bool IsGrouping;

  Expr **getSubExprs() const { return SubExprs; }  
public:
  TupleExpr(SourceLoc lparenloc, Expr **subexprs, Identifier *subexprnames,
            unsigned numsubexprs, SourceLoc rparenloc, bool isGrouping,
            TypeJudgement Ty = TypeJudgement())
    : Expr(ExprKind::Tuple, Ty), LParenLoc(lparenloc), SubExprs(subexprs),
      SubExprNames(subexprnames), NumSubExprs(numsubexprs),
      RParenLoc(rparenloc), IsGrouping(isGrouping) {
    assert((!isGrouping ||
            (NumSubExprs == 1 && getElementName(0).empty() && SubExprs[0])) &&
           "Invalid grouping paren");
    assert(lparenloc.isValid() == rparenloc.isValid() &&
           "Mismatched parenthesis location information validity");
  }

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  SourceRange getSourceRange() const;

  unsigned getNumElements() const { return NumSubExprs; }

  ArrayRef<Expr*> getElements() const {
    return ArrayRef<Expr*>(SubExprs, NumSubExprs);
  }
  Expr *getElement(unsigned i) const {
    assert(i < NumSubExprs && "Invalid element index");
    return getSubExprs()[i];
  }
  void setElement(unsigned i, Expr *e) {
    assert(i < NumSubExprs && "Invalid element index");
    getSubExprs()[i] = e;
  }

  bool hasElementNames() const { return SubExprNames; }

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
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Tuple; }
};

/// UnresolvedDotExpr - A field access (foo.bar) on an expression with dependent
/// type.
class UnresolvedDotExpr : public Expr {
  Expr *SubExpr;
  SourceLoc DotLoc;
  Identifier Name;
  SourceLoc NameLoc;
  
  /// ResolvedDecl - If the name refers to any local or top-level declarations,
  /// the name binder fills them in here.
  ArrayRef<ValueDecl*> ResolvedDecls;

public:
  UnresolvedDotExpr(Expr *subexpr, SourceLoc dotloc, Identifier name,
                    SourceLoc nameloc)
  : Expr(ExprKind::UnresolvedDot), SubExpr(subexpr), DotLoc(dotloc),
    Name(name), NameLoc(nameloc) {}
  
  SourceLoc getLoc() const { return NameLoc; }
  
  SourceRange getSourceRange() const {
    SourceLoc Start;
    if (SubExpr)
      Start = SubExpr->getStartLoc();
    else
      Start = DotLoc;
    
    return SourceRange(Start, NameLoc);
  }
  
  SourceLoc getDotLoc() const { return DotLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *e) { SubExpr = e; }

  Identifier getName() const { return Name; }
  SourceLoc getNameLoc() const { return NameLoc; }

  ArrayRef<ValueDecl*> getResolvedDecls() const { return ResolvedDecls; }
  void setResolvedDecls(ArrayRef<ValueDecl*> Decls) {
    assert(ResolvedDecls.empty());
    ResolvedDecls = Decls;
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDotExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::UnresolvedDot;
  }
};

/// TupleElementExpr - Refer to an element of a tuple, e.g. "(1,2).field0".
class TupleElementExpr : public Expr {
  Expr *SubExpr;
  SourceLoc DotLoc;
  unsigned FieldNo;
  SourceLoc NameLoc;
  
public:
  TupleElementExpr(Expr *subexpr, SourceLoc dotloc, unsigned fieldno,
                   SourceLoc nameloc, TypeJudgement ty = TypeJudgement())
  : Expr(ExprKind::TupleElement, ty), SubExpr(subexpr), DotLoc(dotloc),
    FieldNo(fieldno), NameLoc(nameloc) {}

  SourceRange getSourceRange() const { 
    return SourceRange(SubExpr->getStartLoc(), NameLoc);
  }
  
  SourceLoc getDotLoc() const { return DotLoc; }
  SourceLoc getLoc() const { return NameLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *e) { SubExpr = e; }

  unsigned getFieldNumber() const { return FieldNo; }
  SourceLoc getNameLoc() const { return NameLoc; }  
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleElementExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::TupleElement;
  }
};

/// TupleShuffleExpr - This represents a permutation of a tuple value to a new
/// tuple type.  The expression's type is known to be a tuple type and the
/// subexpression is known to have a tuple type as well.
class TupleShuffleExpr : public Expr {
  Expr *SubExpr;
  
  /// This contains an entry for each element in the Expr type.  Each element
  /// specifies which index from the SubExpr that the destination element gets.
  /// If the element value is -1, then the destination value gets the default
  /// initializer for that tuple element value.
  ArrayRef<int> ElementMapping;
  
public:
  TupleShuffleExpr(Expr *subExpr, ArrayRef<int> elementMapping, Type Ty)
    : Expr(ExprKind::TupleShuffle, TypeJudgement(Ty, ValueKind::RValue)),
      SubExpr(subExpr), ElementMapping(elementMapping) {}

  SourceRange getSourceRange() const { return SubExpr->getSourceRange(); }
  
  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *e) { SubExpr = e; }
  ArrayRef<int> getElementMapping() const { return ElementMapping; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleShuffleExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::TupleShuffle;
  }
};

/// LoadExpr - An implicitly-emitted lvalue-to-rvalue conversion.
class LoadExpr : public Expr {
  Expr *SubExpr;

public:
  LoadExpr(Expr *SubExpr)
    : Expr(ExprKind::Load,
           TypeJudgement(SubExpr->getType(), ValueKind::RValue)),
      SubExpr(SubExpr) {}

  SourceRange getSourceRange() const { return SubExpr->getSourceRange(); }

  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *E) { SubExpr = E; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const LoadExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Load; }
};
  
/// SequenceExpr - a series of expressions which should be evaluated
/// sequentially, e.g. foo()  bar().
class SequenceExpr : public Expr {
  unsigned NumElements;

  Expr **getSubExprs() { return reinterpret_cast<Expr **>(this + 1); }
  Expr * const *getSubExprs() const {
    return const_cast<SequenceExpr*>(this)->getSubExprs();
  }

  SequenceExpr(ArrayRef<Expr*> elements)
    : Expr(ExprKind::Sequence), NumElements(elements.size()) {
    assert(NumElements > 0 && "zero-length sequence!");
    memcpy(getSubExprs(), elements.data(), elements.size() * sizeof(Expr*));
  }

public:
  static SequenceExpr *create(ASTContext &ctx, ArrayRef<Expr*> elements);

  SourceRange getSourceRange() const { 
    return SourceRange(getElements()[0]->getStartLoc(),
                       getElements()[getNumElements() - 1]->getEndLoc());
  }
  
  unsigned getNumElements() const { return NumElements; }

  ArrayRef<Expr*> getElements() const {
    return ArrayRef<Expr*>(getSubExprs(), NumElements);
  }

  Expr *getElement(unsigned i) const {
    assert(i < NumElements);
    return getSubExprs()[i];
  }
  void setElement(unsigned i, Expr *e) {
    assert(i < NumElements);
    getSubExprs()[i] = e;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SequenceExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Sequence; }
};

/// FuncExpr - An explicit unnamed func definition, which can optionally
/// have named arguments.
///    e.g.  func(a : int) -> int { return a+1 }
class FuncExpr : public Expr, public DeclContext {
  SourceLoc FuncLoc;
  
  ArrayRef<ArgDecl*> NamedArgs;
  BraceStmt *Body;
  
public:
  FuncExpr(SourceLoc FuncLoc, Type FnType, ArrayRef<ArgDecl*> NamedArgs, 
           BraceStmt *Body, DeclContext *Parent)
    : Expr(ExprKind::Func, TypeJudgement(FnType, ValueKind::RValue)),
      DeclContext(DeclContextKind::FuncExpr, Parent),
      FuncLoc(FuncLoc), NamedArgs(NamedArgs), Body(Body) {}

  SourceRange getSourceRange() const;
  SourceLoc getLoc() const { return FuncLoc; }

  /// Returns the location of the 'func' keyword.
  SourceLoc getFuncLoc() const { return FuncLoc; }

  ArrayRef<ArgDecl*> getNamedArgs() const { return NamedArgs; }
  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *S) { Body = S; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FuncExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Func; }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::FuncExpr;
  }

};
  
/// ClosureExpr - An expression which is implicitly created by using an
/// expression in a function context where the expression's type matches the
/// result of the function.  The Decl list indicates which decls the formal
/// arguments are bound to.
class ClosureExpr : public Expr {
  Expr *Input;
  
public:
  ClosureExpr(Expr *input, Type ResultTy)
    : Expr(ExprKind::Closure, TypeJudgement(ResultTy, ValueKind::RValue)),
      Input(input) {}

  Expr *getInput() const { return Input; }
  void setInput(Expr *e) { Input = e; }

  SourceRange getSourceRange() const { return Input->getSourceRange(); }
  
  /// getNumArgs - Return the number of arguments that this closure expr takes.
  unsigned getNumArgs() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClosureExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Closure; }
};
  
class AnonClosureArgExpr : public Expr {
  unsigned ArgNo;
  SourceLoc Loc;
  
public:
  AnonClosureArgExpr(unsigned argNo, SourceLoc loc)
    : Expr(ExprKind::AnonClosureArg), ArgNo(argNo), Loc(loc) {}

  SourceRange getSourceRange() const { return Loc; }

  unsigned getArgNumber() const { return ArgNo; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const AnonClosureArgExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::AnonClosureArg;
  }
};
  
/// ApplyExpr - Superclass of various function calls, which apply an argument to
/// a function to get a result.
class ApplyExpr : public Expr {
  /// Fn - The function being called.
  Expr *Fn;

  /// Argument - The one argument being passed to it.
  Expr *Arg;

protected:
  ApplyExpr(ExprKind Kind, Expr *Fn, Expr *Arg,
            TypeJudgement Ty = TypeJudgement())
    : Expr(Kind, Ty), Fn(Fn), Arg(Arg) {
    assert(classof((Expr*)this) && "ApplyExpr::classof out of date");
  }

public:
  Expr *getFn() const { return Fn; }
  void setFn(Expr *e) { Fn = e; }

  Expr *getArg() const { return Arg; }
  void setArg(Expr *e) { Arg = e; }

  ValueDecl *getCalledValue() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ApplyExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_ApplyExpr &&
           E->getKind() <= ExprKind::Last_ApplyExpr;
  }
};
  
/// CallExpr - Application of an argument to a function, which occurs
/// syntactically through juxtaposition with a TupleExpr whose
/// leading '(' is unspaced.
class CallExpr : public ApplyExpr {
public:
  CallExpr(Expr *Fn, Expr *Arg, TypeJudgement Ty)
    : ApplyExpr(ExprKind::Call, Fn, Arg, Ty) {}

  SourceRange getSourceRange() const {
    return SourceRange(getFn()->getStartLoc(), getArg()->getEndLoc()); 
  }  
  SourceLoc getLoc() const { return getArg()->getStartLoc(); }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const CallExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Call; }
};
  
/// UnaryExpr - Prefix unary expressions like '!y'.
class UnaryExpr : public ApplyExpr {
public:
  UnaryExpr(Expr *Fn, Expr *Arg, TypeJudgement Ty = TypeJudgement())
    : ApplyExpr(ExprKind::Unary, Fn, Arg, Ty) {}

  SourceLoc getLoc() const { return getFn()->getStartLoc(); }
  
  SourceRange getSourceRange() const {
    return SourceRange(getFn()->getStartLoc(), getArg()->getEndLoc()); 
  }  

  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Unary; }
};
  
/// BinaryExpr - Infix binary expressions like 'x+y'.  The argument is always
/// an implicit tuple expression of the type expected by the function.
class BinaryExpr : public ApplyExpr {
public:
  BinaryExpr(Expr *Fn, TupleExpr *Arg, TypeJudgement Ty = TypeJudgement())
    : ApplyExpr(ExprKind::Binary, Fn, Arg, Ty) {}

  /// getArgTuple - The argument is always a tuple literal.  This accessor
  /// reinterprets it properly.
  TupleExpr *getArgTuple() const {
    return cast<TupleExpr>(getArg());
  }

  SourceLoc getLoc() const { return getFn()->getLoc(); }
                 
  SourceRange getSourceRange() const {
    return getArgTuple()->getSourceRange();
  }  

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BinaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Binary; }
};

/// ProtocolElementExpr - Refer to an element of a protocol, e.g. P.x.  'x' is
/// modeled as a DeclRefExpr on the field's decl.
///
class ProtocolElementExpr : public ApplyExpr {
  SourceLoc DotLoc;
  
public:
  ProtocolElementExpr(Expr *FnExpr, SourceLoc DotLoc, Expr *BaseExpr,
                      TypeJudgement Ty = TypeJudgement())
  : ApplyExpr(ExprKind::ProtocolElement, FnExpr, BaseExpr, Ty), DotLoc(DotLoc) {
  }

  SourceLoc getDotLoc() const { return DotLoc; }
  SourceLoc getLoc() const { return getArg()->getStartLoc(); }
  
  SourceRange getSourceRange() const {
    return SourceRange(getFn()->getStartLoc(), getArg()->getEndLoc());
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ProtocolElementExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ProtocolElement;
  }
};


  
} // end namespace swift

#endif
