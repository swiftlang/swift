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
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class ASTContext;
  class Type;
  class ValueDecl;
  class Decl;
  class Pattern;
  class SubscriptDecl;
  class Stmt;
  class BraceStmt;
  class TypeAliasDecl;
  class ASTWalker;
  class VarDecl;

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
  Type Ty;

protected:
  Expr(ExprKind Kind, Type Ty = Type()) : Kind(Kind), Ty(Ty) {}

public:
  /// getKind - Return the kind of this expression.
  ExprKind getKind() const { return Kind; }

  /// getType - Return the type of this expression.
  Type getType() const { return Ty; }

  /// setType - Sets the type of this expression.
  void setType(Type T) { Ty = T; }

  /// \brief Return the source range of the expression.
  SourceRange getSourceRange() const;
  
  /// getStartLoc - Return the location of the start of the expression.
  SourceLoc getStartLoc() const { return getSourceRange().Start; }

  /// \brief Retrieve the location of the end of the expression.
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  
  /// getLoc - Return the caret location of this expression.
  SourceLoc getLoc() const;

  /// getSemanticsProvidingExpr - Find the smallest subexpression
  /// which obeys the property that evaluating it is exactly
  /// equivalent to evaluating this expression.
  ///
  /// Looks through parentheses.  Would not look through something
  /// like '(foo(), x:bar(), baz()).x'.
  Expr *getSemanticsProvidingExpr();

  /// getValueProvidingExpr - Find the smallest subexpression which is
  /// responsible for generating the value of this expression.
  /// Evaluating the result is not necessarily equivalent to
  /// evaluating this expression because of potential missing
  /// side-effects (which may influence the returned value).
  Expr *getValueProvidingExpr();

  /// walk - This recursively walks the AST rooted at this expression.
  Expr *walk(ASTWalker &walker);
  Expr *walk(ASTWalker &&walker) { return walk(walker); }
  
  /// isImplicit - Determines whether this expression was implicitly-generated,
  /// rather than explicitly written in the AST.
  bool isImplicit() const;
  
  void dump() const;
  void print(raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Expr *) { return true; }

  enum { Alignment = 8U };

  // Only allow allocation of Exprs using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = Expr::Alignment);  

  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
};

  
/// ErrorExpr - Represents a semantically erroneous subexpression in the AST,
/// typically this will have an ErrorType.
class ErrorExpr : public Expr {
  SourceRange Range;
public:
  ErrorExpr(SourceRange Range, Type Ty = Type())
    : Expr(ExprKind::Error, Ty), Range(Range) {}

  SourceRange getSourceRange() const { return Range; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ErrorExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Error;
  }
};

/// LiteralExpr - Common base class between the literals.
class LiteralExpr : public Expr {
public:
  LiteralExpr(ExprKind Kind) : Expr(Kind) {}
  
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const LiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_LiteralExpr &&
           E->getKind() <= ExprKind::Last_LiteralExpr;
  }
};

/// IntegerLiteralExpr - Integer literal, like '4'.  After semantic analysis
/// assigns types, this is guaranteed to only have a BuiltinIntegerType.
class IntegerLiteralExpr : public LiteralExpr {
  StringRef Val;  // Use StringRef instead of APInt, APInt leaks.
  SourceLoc Loc;

public:
  IntegerLiteralExpr(StringRef Val, SourceLoc Loc)
    : LiteralExpr(ExprKind::IntegerLiteral), Val(Val), Loc(Loc) {}
  
  APInt getValue() const;

  StringRef getText() const { return Val; }
  
  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const IntegerLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::IntegerLiteral;
  }
};

/// FloatLiteralExpr - Floating point literal, like '4.0'.  After semantic
/// analysis assigns types, this is guaranteed to only have a
/// BuiltinFloatingPointType.
class FloatLiteralExpr : public LiteralExpr {
  StringRef Val; // Use StringRef instead of APFloat, APFloat leaks.
  SourceLoc Loc;

public:
  FloatLiteralExpr(StringRef Val, SourceLoc Loc)
    : LiteralExpr(ExprKind::FloatLiteral), Val(Val), Loc(Loc) {}

  APFloat getValue() const;

  StringRef getText() const { return Val; }
  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FloatLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::FloatLiteral;
  }
};

  
/// CharacterLiteral - Character literal, like 'x'.  After semantic analysis
/// assigns types, this is guaranteed to only have a 32-bit BuiltinIntegerType.
class CharacterLiteralExpr : public LiteralExpr {
  uint32_t Val;
  SourceLoc Loc;
public:
  CharacterLiteralExpr(uint32_t Val, SourceLoc Loc)
    : LiteralExpr(ExprKind::CharacterLiteral), Val(Val), Loc(Loc) {}
  
  uint32_t getValue() const { return Val; }
  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const CharacterLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::CharacterLiteral;
  }
};
  
/// StringLiteralExpr - String literal, like '"foo"'.  After semantic
/// analysis assigns types, this is guaranteed to only have a
/// BuiltinRawPointerType.
class StringLiteralExpr : public LiteralExpr {
  StringRef Val;
  SourceLoc Loc;
  
public:
  StringLiteralExpr(StringRef Val, SourceLoc Loc)
    : LiteralExpr(ExprKind::StringLiteral), Val(Val), Loc(Loc) {}
  
  StringRef getValue() const { return Val; }
  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const StringLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::StringLiteral;
  }
};

/// InterpolatedStringLiteral - An interpolated string literal.
///
/// An interpolated string literal mixes expressions (which are evaluated and
/// converted into string form) within a string literal.
///
/// \code
/// "[\(min)..\(max)]"
/// \endcode
class InterpolatedStringLiteralExpr : public LiteralExpr {
  SourceLoc Loc;
  MutableArrayRef<Expr *> Segments;
  Expr *SemanticExpr;
  
public:
  InterpolatedStringLiteralExpr(SourceLoc Loc, MutableArrayRef<Expr *> Segments)
    : LiteralExpr(ExprKind::InterpolatedStringLiteral), Loc(Loc), 
      Segments(Segments), SemanticExpr() { }
  
  MutableArrayRef<Expr *> getSegments() { return Segments; }
  ArrayRef<Expr *> getSegments() const { return Segments; }
  
  /// \brief Retrieve the expression that actually evaluates the resulting
  /// string, typically with a series of '+' operations.
  Expr *getSemanticExpr() const { return SemanticExpr; }
  void setSemanticExpr(Expr *SE) { SemanticExpr = SE; }
  
  SourceRange getSourceRange() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const InterpolatedStringLiteralExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::InterpolatedStringLiteral;
  }
};

/// DeclRefExpr - A reference to a value, "x".
class DeclRefExpr : public Expr {
  ValueDecl *D;
  SourceLoc Loc;

public:
  DeclRefExpr(ValueDecl *D, SourceLoc Loc, Type Ty = Type())
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
///
/// This is an abstract class that covers the various different kinds of
/// overload sets.
class OverloadSetRefExpr : public Expr {
  ArrayRef<ValueDecl*> Decls;

protected:
  OverloadSetRefExpr(ExprKind Kind, ArrayRef<ValueDecl*> decls, Type Ty)
    : Expr(Kind, Ty), Decls(decls) {}

public:
  ArrayRef<ValueDecl*> getDecls() const { return Decls; }
  
  /// getBaseType - Determine the type of the base object provided for the
  /// given overload set, which is only non-null when dealing with an overloaded
  /// member reference.
  Type getBaseType() const;
  
  /// createFilteredWithCopy - Given a subset of the declarations in the given
  /// overloaded reference expression, return a new expression of the same
  /// form but with the restricted set of declarations. This is equivalent
  /// to calling createWithCopy() on the appropriate subclass of
  /// OverloadSetRefExpr with arguments derived from the current expression.
  Expr *createFilteredWithCopy(ArrayRef<ValueDecl *> Decls);
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OverloadSetRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_OverloadSetRefExpr &&
           E->getKind() <= ExprKind::Last_OverloadSetRefExpr;
  }
};

/// OverloadedDeclRefExpr - A reference to an overloaded name that should
/// eventually be resolved (by overload resolution) to a value reference.
class OverloadedDeclRefExpr : public OverloadSetRefExpr {
  SourceLoc Loc;

public:
  OverloadedDeclRefExpr(ArrayRef<ValueDecl*> Decls, SourceLoc Loc, Type Ty)
    : OverloadSetRefExpr(ExprKind::OverloadedDeclRef, Decls, Ty), Loc(Loc) { }
  
  SourceLoc getLoc() const { return Loc; }
  SourceRange getSourceRange() const { return Loc; }

  /// createWithCopy - Create and return a new OverloadedDeclRefExpr or a new
  /// DeclRefExpr (if the list of decls has a single entry) from the specified
  /// (non-empty) list of decls.  If we end up creating an overload set, this
  /// method handles copying the list of decls into ASTContext memory.
  static Expr *createWithCopy(ArrayRef<ValueDecl*> Decls, SourceLoc Loc);
  
  template <typename T>
  static Expr *createWithCopy(ArrayRef<T*> Decls, SourceLoc Loc) {
    llvm::SmallVector<ValueDecl*, 4> ValueDecls(Decls.begin(), Decls.end());
    return createWithCopy(ValueDecls, Loc);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const OverloadedDeclRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::OverloadedDeclRef;
  }
};

/// OverloadedMemberRefExpr - A reference to an overloaded name that is a
/// member, relative to some base expression, that will eventually be
/// resolved to some kind of member-reference expression.
class OverloadedMemberRefExpr : public OverloadSetRefExpr {
  Expr *SubExpr;
  SourceLoc DotLoc;
  SourceLoc MemberLoc;
  
public:
  OverloadedMemberRefExpr(Expr *SubExpr, SourceLoc DotLoc,
                          ArrayRef<ValueDecl *> Decls, SourceLoc MemberLoc,
                          Type Ty)
    : OverloadSetRefExpr(ExprKind::OverloadedMemberRef, Decls, Ty),
      SubExpr(SubExpr), DotLoc(DotLoc), MemberLoc(MemberLoc) { }

  SourceLoc getDotLoc() const { return DotLoc; }
  SourceLoc getMemberLoc() const { return MemberLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *E) { SubExpr = E; }
  
  SourceLoc getLoc() const { return MemberLoc; }
  SourceLoc getStartLoc() const {
    return DotLoc.isValid()? SubExpr->getStartLoc() : MemberLoc;
  }
  SourceLoc getEndLoc() const { return MemberLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(getStartLoc(), MemberLoc);
  }

  /// createWithCopy - Create and return a new OverloadedMemberRefExpr or a new
  /// DotSyntaxCallExpr (if the list of decls has a single entry) from the
  /// specified (non-empty) list of decls and with the given base.  If we end up
  /// creating an overload set, this method handles copying the list of decls
  /// into ASTContext memory.
  static Expr *createWithCopy(Expr *Base, SourceLoc DotLoc,
                              ArrayRef<ValueDecl*> Decls,
                              SourceLoc MemberLoc);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const OverloadedMemberRefExpr *) { return true; }
  static bool classof(const Expr *E) {
  return E->getKind() == ExprKind::OverloadedMemberRef;
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

/// MemberRefExpr - This represents 'a.b' where we are referring to a member
/// of a type.
///
/// FIXME: This is only used for property accesses right now, because structs
/// are tuples and, therefore, their elements are referenced via a
/// TupleElementExpr. This will change when structs become "real" types.
class MemberRefExpr : public Expr {
  Expr *Base;
  VarDecl *Value;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  
public:  
  MemberRefExpr(Expr *Base, SourceLoc DotLoc, VarDecl *Value,
                SourceLoc NameLoc);
  Expr *getBase() const { return Base; }
  VarDecl *getDecl() const { return Value; }
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getDotLoc() const { return DotLoc; }
  
  void setBase(Expr *E) { Base = E; }
  
  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const {
    if (Base->isImplicit())
      return SourceRange(NameLoc);
  
    return SourceRange(Base->getStartLoc(), NameLoc);
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const MemberRefExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::MemberRef;
  }
};
  

/// UnresolvedMemberExpr - This represents '.foo', an unresolved reference to a
/// member, which is to be resolved with context sensitive type information into
/// bar.foo.  These always have dependent type.
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
  
/// ParenExpr - A parenthesized expression like '(x+x)'.  Syntactically,
/// this is just a TupleExpr with exactly one element that has no label.
/// Semantically, however, it serves only as grouping parentheses and
/// does not form an expression of tuple type (unless the sub-expression
/// has tuple type, of course).
class ParenExpr : public Expr {
  SourceLoc LParenLoc, RParenLoc;
  Expr *SubExpr;

public:
  ParenExpr(SourceLoc lploc, Expr *subExpr, SourceLoc rploc,
            Type ty = Type())
    : Expr(ExprKind::Paren, ty), LParenLoc(lploc), RParenLoc(rploc),
      SubExpr(subExpr) {
    // We just assert that these are always valid; it's not clear why
    // you'd ever construct something where that's not true.
    assert(lploc.isValid() && rploc.isValid());
  }

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  SourceLoc getLoc() const { return SubExpr->getLoc(); }
  SourceRange getSourceRange() const {
    return SourceRange(LParenLoc, RParenLoc);
  }

  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *E) { SubExpr = E; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ParenExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Paren; }
};
  
/// TupleExpr - Parenthesized expressions like '(a=x+x)' and '(x, y, 4)'.  Also
/// used to represent the operands to a binary operator.  Note that
/// expressions like '(4)' are represented with a ParenExpr.
class TupleExpr : public Expr {
  SourceLoc LParenLoc;
  SourceLoc RParenLoc;
  /// SubExprs - Elements of these can be set to null to get the default init
  /// value for the tuple element.
  MutableArrayRef<Expr *> SubExprs;
  /// SubExprNames - Can be null if no names.  Otherwise length = SubExpr.size()
  Identifier *SubExprNames;
  
public:
  TupleExpr(SourceLoc LParenLoc, MutableArrayRef<Expr *> SubExprs,
            Identifier *SubExprNames, SourceLoc RParenLoc,
            Type Ty = Type())
    : Expr(ExprKind::Tuple, Ty), LParenLoc(LParenLoc), RParenLoc(RParenLoc),
      SubExprs(SubExprs), SubExprNames(SubExprNames) {
    assert((LParenLoc.isValid() ||
            (!RParenLoc.isValid() && getNumElements() == 2)) &&
           "Mismatched parenthesis location information validity");
  }

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  SourceRange getSourceRange() const;

  MutableArrayRef<Expr*> getElements() {
    return SubExprs;
  }

  ArrayRef<Expr*> getElements() const {
    return SubExprs;
  }
  
  unsigned getNumElements() const { return SubExprs.size(); }
  
  Expr *getElement(unsigned i) const {
    return SubExprs[i];
  }
  void setElement(unsigned i, Expr *e) {
    SubExprs[i] = e;
  }

  bool hasElementNames() const { return SubExprNames; }

  Identifier getElementName(unsigned i) const {
    return SubExprNames ? SubExprNames[i] : Identifier();
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Tuple; }
};

/// SubscriptExpr - Subscripting expressions like a[i] that refer to an element
/// within a container.
///
/// There is no built-in subscripting in the language. Rather, a fully
/// type-checked and well-formed subscript expression refers to a subscript
/// declaration, which provides a getter and (optionally) a setter that will
/// be used to perform reads/writes.
class SubscriptExpr : public Expr {
  SubscriptDecl *D;
  SourceRange Brackets;
  Expr *Base;
  Expr *Index;
  
public:
  SubscriptExpr(Expr *Base, SourceLoc LBracketLoc, Expr *Index,
                SourceLoc RBracketLoc, SubscriptDecl *D = 0);
  
  /// getBase - Retrieve the base of the subscript expression, i.e., the
  /// value being indexed.
  Expr *getBase() const { return Base; }
  void setBase(Expr *E) { Base = E; }
  
  /// getBase - Retrieve the index of the subscript expression, i.e., the
  /// "offset" into the base value.
  Expr *getIndex() const { return Index; }
  void setIndex(Expr *E) { Index = E; }
  
  /// hasDecl - Determine whether subscript operation has a known underlying
  /// subscript declaration or not.
  bool hasDecl() const { return D != 0; }
  
  /// getDecl - Retrieve the subscript declaration that this subscripting
  /// operation refers to. Only valid when \c hasDecl() is true.
  SubscriptDecl *getDecl() const {
    assert(hasDecl() && "No subscript declaration known!");
    return D;
  }
  void setDecl(SubscriptDecl *D) { this->D = D; }
  
  SourceLoc getLBracketLoc() const { return Brackets.Start; }
  SourceLoc getRBracketLoc() const { return Brackets.End; }
  
  SourceRange getSourceRange() const {
    return SourceRange(Base->getStartLoc(), Brackets.End);
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const SubscriptExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Subscript;
  }
};

/// OverloadedSubscriptExpr - Subscripting expressions like a[i] that refer to
/// an element within a container, for which overload resolution has found
/// multiple potential subscript declarations that may apply.
///
/// Instances of OverloadedSubscriptExpr are mapped down to SubscriptExpr
/// instances by type-checking.
class OverloadedSubscriptExpr : public Expr {
  SourceRange Brackets;
  ArrayRef<ValueDecl *> Decls;
  Expr *Base;
  Expr *Index;
  
  OverloadedSubscriptExpr(Expr *Base, ArrayRef<ValueDecl *> Decls,
                          SourceLoc LBracketLoc, Expr *Index,
                          SourceLoc RBracketLoc, Type Ty)
    : Expr(ExprKind::OverloadedSubscript, Ty),
      Brackets(LBracketLoc, RBracketLoc), Decls(Decls), Base(Base),
      Index(Index) { }
  
public:
  Expr *getBase() const { return Base; }
  Expr *getIndex() const { return Index; }
  
  ArrayRef<ValueDecl *> getDecls() const { return Decls; }
  
  SourceLoc getLBracketLoc() const { return Brackets.Start; }
  SourceLoc getRBracketLoc() const { return Brackets.End; }
  
  SourceLoc getLoc() const { return getLBracketLoc(); }
  SourceLoc getStartLoc() const { return getBase()->getStartLoc(); }
  SourceLoc getEndLoc() const { return getRBracketLoc(); }
  
  SourceRange getSourceRange() const {
    return SourceRange(getBase()->getStartLoc(), getRBracketLoc());
  }
  
  /// createWithCopy - Create and return a new OverloadedSubscriptExpr or a
  /// new SubscriptExpr (if the list of decls has a single entry) from the
  /// specified (non-empty) list of decls and with the given base/index.
  static Expr *createWithCopy(Expr *Base, ArrayRef<ValueDecl*> Decls,
                              SourceLoc LBracketLoc, Expr *Index,
                              SourceLoc RBracketLoc);
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OverloadedSubscriptExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::OverloadedSubscript;
  }
};

/// UnresolvedDotExpr - A field access (foo.bar) on an expression with dependent
/// type.
class UnresolvedDotExpr : public Expr {
  Expr *SubExpr;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  Identifier Name;
public:
  UnresolvedDotExpr(Expr *subexpr, SourceLoc dotloc, Identifier name,
                    SourceLoc nameloc)
  : Expr(ExprKind::UnresolvedDot), SubExpr(subexpr), DotLoc(dotloc),
    NameLoc(nameloc), Name(name) {}
  
  SourceLoc getLoc() const { return NameLoc; }
  
  SourceRange getSourceRange() const {
    return SourceRange(SubExpr->getStartLoc(), NameLoc);
  }
  
  SourceLoc getDotLoc() const { return DotLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *e) { SubExpr = e; }

  Identifier getName() const { return Name; }
  SourceLoc getNameLoc() const { return NameLoc; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedDotExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::UnresolvedDot;
  }
};

/// ModuleExpr - Reference a module by name.  The module being referenced is
/// captured in the type of the expression, which is always a ModuleType.
class ModuleExpr : public Expr {
  SourceLoc Loc;
  
public:
  ModuleExpr(SourceLoc Loc, Type Ty)
  : Expr(ExprKind::Module, Ty), Loc(Loc) {}
  
  SourceRange getSourceRange() const { return SourceRange(Loc, Loc); }
  SourceLoc getLoc() const { return Loc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ModuleExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Module;
  }
};


/// TupleElementExpr - Common base class of syntactic forms that access tuple
/// elements.
class TupleElementExpr : public Expr {
  Expr *SubExpr;
  SourceLoc NameLoc;
  unsigned FieldNo;
  
protected:
  TupleElementExpr(ExprKind Kind, Expr *SubExpr, unsigned FieldNo,
                   SourceLoc NameLoc, Type Ty)
    : Expr(Kind, Ty), SubExpr(SubExpr), NameLoc(NameLoc), FieldNo(FieldNo) {}
public:

  SourceLoc getLoc() const { return NameLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *e) { SubExpr = e; }

  unsigned getFieldNumber() const { return FieldNo; }
  SourceLoc getNameLoc() const { return NameLoc; }  
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleElementExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::SyntacticTupleElement ||
           E->getKind() == ExprKind::ImplicitThisTupleElement;
  }
};
  
  
/// SyntacticTupleElementExpr - Dot syntact used to refer to an element of a tuple,
/// e.g. "(1,field=2).field".
class SyntacticTupleElementExpr : public TupleElementExpr {
  SourceLoc DotLoc;
public:
  SourceLoc getDotLoc() const { return DotLoc; }

  SourceRange getSourceRange() const { 
    return SourceRange(getBase()->getStartLoc(), getNameLoc());
  }

  SyntacticTupleElementExpr(Expr *SubExpr, SourceLoc DotLoc,
                            unsigned FieldNo, SourceLoc NameLoc, Type Ty)
    : TupleElementExpr(ExprKind::SyntacticTupleElement, SubExpr, FieldNo,
                       NameLoc, Ty),
      DotLoc(DotLoc) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SyntacticTupleElementExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::SyntacticTupleElement;
  }
};

/// ImplicitThisTupleElementExpr - Reference to a tuple element inside of a
/// context that has an implicit this member.  We represent the AST with an
/// explicit reference to 'this', but use this node so that clients have
/// accurate source ranges and other location information.
class ImplicitThisTupleElementExpr : public TupleElementExpr {
public:
  
  // The name is the only thing that was written in the code.
  SourceRange getSourceRange() const { 
    return SourceRange(getNameLoc(), getNameLoc());
  }
  
  ImplicitThisTupleElementExpr(Expr *SubExpr,
                               unsigned FieldNo, SourceLoc NameLoc, Type Ty)
    : TupleElementExpr(ExprKind::ImplicitThisTupleElement, SubExpr, FieldNo,
                     NameLoc, Ty) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ImplicitThisTupleElementExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ImplicitThisTupleElement;
  }
};


/// ImplicitConversionExpr - An abstract class for expressions which
/// implicitly convert the value of an expression in some way.
class ImplicitConversionExpr : public Expr {
  Expr *SubExpr;

protected:
  ImplicitConversionExpr(ExprKind kind, Expr *subExpr, Type ty)
    : Expr(kind, ty), SubExpr(subExpr) {}

public:
  SourceRange getSourceRange() const { return SubExpr->getSourceRange(); }
  SourceLoc getLoc() const { return SubExpr->getLoc(); }

  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *e) { SubExpr = e; }

  static bool classof(const ImplicitConversionExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_ImplicitConversionExpr &&
           E->getKind() <= ExprKind::Last_ImplicitConversionExpr;
  }
};

/// TupleShuffleExpr - This represents a permutation of a tuple value to a new
/// tuple type.  The expression's type is known to be a tuple type and the
/// subexpression is known to have a tuple type as well.
class TupleShuffleExpr : public ImplicitConversionExpr {
  /// This contains an entry for each element in the Expr type.  Each element
  /// specifies which index from the SubExpr that the destination element gets.
  /// If the element value is -1, then the destination value gets the default
  /// initializer for that tuple element value.
  ArrayRef<int> ElementMapping;
  
public:
  TupleShuffleExpr(Expr *subExpr, ArrayRef<int> elementMapping, Type ty)
    : ImplicitConversionExpr(ExprKind::TupleShuffle, subExpr, ty),
      ElementMapping(elementMapping) {}

  ArrayRef<int> getElementMapping() const { return ElementMapping; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleShuffleExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::TupleShuffle;
  }
};

/// LoadExpr - Turn an l-value into an r-value by performing a "load"
/// operation.  This operation may actually be a logical operation,
/// i.e. one implemented using a call to a potentially user-defined
/// function instead of a simple memory transaction.
class LoadExpr : public ImplicitConversionExpr {
public:
  LoadExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::Load, subExpr, type) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const LoadExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Load; }
};

/// MaterializeExpr - Turn an r-value into an l-value by placing it in
/// temporary memory.
class MaterializeExpr : public ImplicitConversionExpr {
public:
  MaterializeExpr(Expr *subExpr, Type ty)
    : ImplicitConversionExpr(ExprKind::Materialize, subExpr, ty) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const MaterializeExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Materialize;
  }
};

/// RequalifyExpr - Change the qualification on an l-value.  The new
/// type always has the same object type as the old type with strictly
/// "more" (i.e. a supertyped set of) qualifiers.
class RequalifyExpr : public ImplicitConversionExpr {
public:
  RequalifyExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::Requalify, subExpr, type) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const RequalifyExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Requalify;
  }
};
  
/// LookThroughOneofExpr - Implicitly look through a 'oneof' type with
/// one enumerator.  This operation may be performed on either an
/// r-value or an l-value.
class LookThroughOneofExpr : public ImplicitConversionExpr {
public:
  LookThroughOneofExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::LookThroughOneof, subExpr, type) {}

  // Implement isa/cast/dyncast/etc.
  static bool classof(const LookThroughOneofExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::LookThroughOneof;
  }
};

/// ParameterRenameExpr - Rename the parameters or return values of a 
/// function type.
class ParameterRenameExpr : public ImplicitConversionExpr {
public:
  ParameterRenameExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::ParameterRename, subExpr, type) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ParameterRenameExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ParameterRename;
  }
};

/// ScalarToTupleExpr - Initialze a tuple with a a scalar.
class ScalarToTupleExpr : public ImplicitConversionExpr {
public:
  ScalarToTupleExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::ScalarToTuple, subExpr, type) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ScalarToTupleExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ScalarToTuple;
  }
};

/// AddressOfExpr - Using the builtin unary '&' operator, convert the
/// given l-value into an explicit l-value.
class AddressOfExpr : public Expr {
  Expr *SubExpr;
  SourceLoc OperLoc;

public:
  AddressOfExpr(SourceLoc operLoc, Expr *subExpr, Type type)
    : Expr(ExprKind::AddressOf, type), SubExpr(subExpr), OperLoc(operLoc) {}

  SourceRange getSourceRange() const {
    return SourceRange(OperLoc, SubExpr->getEndLoc());
  }
  SourceLoc getLoc() const { return OperLoc; }

  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *e) { SubExpr = e; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const AddressOfExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::AddressOf;
  }
};

/// SequenceExpr - A list of binary operations which has not yet been
/// folded into a tree.  The operands all have even indices, while the
/// subexpressions with odd indices are all (potentially overloaded)
/// references to binary operators.
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

  MutableArrayRef<Expr*> getElements() {
    return MutableArrayRef<Expr*>(getSubExprs(), NumElements);
  }

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

/// CapturingExpr - a FuncExpr or a ClosureExpr; always returns something
/// of function type, and can capture variables from an enclosing scope.
class CapturingExpr : public Expr, public DeclContext {
  ArrayRef<ValueDecl*> Captures;
  bool IsNotCaptured;

public:
  CapturingExpr(ExprKind Kind, Type FnType, DeclContextKind DCKind,
                DeclContext *Parent)
    : Expr(Kind, FnType), DeclContext(DCKind, Parent), IsNotCaptured(false) {}

  ArrayRef<ValueDecl*> getCaptures() { return Captures; }
  void setCaptures(ArrayRef<ValueDecl*> C) { Captures = C; }

  bool isNotCaptured() { return IsNotCaptured; }
  void setIsNotCaptured(bool v) { IsNotCaptured = v; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const CapturingExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_CapturingExpr &&
           E->getKind() <= ExprKind::Last_CapturingExpr;
  }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::CapturingExpr;
  }
};

/// FuncExpr - An explicit unnamed func definition, which can optionally
/// have named arguments.
///    e.g.  func(a : int) -> int { return a+1 }
class FuncExpr : public CapturingExpr {
  SourceLoc FuncLoc;
  unsigned NumPatterns;
  
  BraceStmt *Body;

  Pattern **getParamsBuffer() {
    return reinterpret_cast<Pattern**>(this+1);
  }
  Pattern * const *getParamsBuffer() const {
    return reinterpret_cast<Pattern*const*>(this+1);
  }
  
  FuncExpr(SourceLoc FuncLoc, unsigned NumPatterns, Type FnType,
           BraceStmt *Body, DeclContext *Parent)
    : CapturingExpr(ExprKind::Func, FnType,
                    DeclContextKind::CapturingExpr, Parent),
      FuncLoc(FuncLoc), NumPatterns(NumPatterns), Body(Body) {}
public:
  static FuncExpr *create(ASTContext &Context, SourceLoc FuncLoc,
                          ArrayRef<Pattern*> Params, Type FnType,
                          BraceStmt *Body, DeclContext *Parent);

  SourceRange getSourceRange() const;
  SourceLoc getLoc() const { return FuncLoc; }

  ArrayRef<Pattern*> getParamPatterns() const {
    return ArrayRef<Pattern*>(getParamsBuffer(), NumPatterns);
  }
  

  /// Returns the location of the 'func' keyword.
  SourceLoc getFuncLoc() const { return FuncLoc; }

  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *S) { Body = S; }

  Type getBodyResultType() const;
 
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FuncExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Func; }
  static bool classof(const DeclContext *DC) {
    return isa<CapturingExpr>(DC) && classof(cast<CapturingExpr>(DC));
  }
  static bool classof(const CapturingExpr *E) { return classof(cast<Expr>(E)); }
};
  
/// ClosureExpr - An expression which is implicitly created by using an
/// expression in a function context where the expression's type matches the
/// result of the function.  This may either be explicit in source or implicitly
/// formed.  Once type checking has completed, ClosureExpr's are known to have
/// FunctionType.
///
class ClosureExpr : public CapturingExpr {
  Expr *Body;
  Pattern *Pat;

public:
  ClosureExpr(ExprKind Kind, Expr *Body, DeclContextKind DCKind,
              DeclContext *Parent, Type ResultTy = Type())
    : CapturingExpr(Kind, ResultTy, DCKind, Parent), Body(Body), Pat(0) {}

  Expr *getBody() const { return Body; }
  void setBody(Expr *e) { Body = e; }

  Pattern *getPattern() { return Pat; }
  const Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *pat) { Pat = pat; }
  ArrayRef<Pattern*> getParamPatterns() const {
    return Pat;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ClosureExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ImplicitClosure ||
           E->getKind() == ExprKind::ExplicitClosure;
  }
  static bool classof(const DeclContext *DC) {
    return isa<CapturingExpr>(DC) && classof(cast<CapturingExpr>(DC));
  }
  static bool classof(const CapturingExpr *E) { return classof(cast<Expr>(E)); }
};

/// ExplicitClosureExpr - An explicitly formed closure expression in braces,
/// e.g. "{ foo() }".  This may contain AnonClosureArgExprs within it that
/// reference the formal arguments of the closure.
class ExplicitClosureExpr : public ClosureExpr {
  SourceLoc LBraceLoc, RBraceLoc;

  ArrayRef<VarDecl*> ParserVarDecls;

public:
  ExplicitClosureExpr(SourceLoc LBraceLoc, DeclContext *Parent,
                      Expr *Body = 0, SourceLoc RBraceLoc = SourceLoc())
    : ClosureExpr(ExprKind::ExplicitClosure, Body,
                  DeclContextKind::CapturingExpr, Parent),
      LBraceLoc(LBraceLoc), RBraceLoc(RBraceLoc) {}
  
  void setRBraceLoc(SourceLoc L) {
    RBraceLoc = L;
  }
  
  SourceRange getSourceRange() const {
    return SourceRange(LBraceLoc, RBraceLoc);
  }

  ArrayRef<VarDecl*> getParserVarDecls() { return ParserVarDecls; }
  void setParserVarDecls(ArrayRef<VarDecl*> decls) {
    ParserVarDecls = decls;
  }
  void GenerateVarDecls(unsigned NumDecls,
                        std::vector<VarDecl*> &Decls,
                        ASTContext &Context);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ExplicitClosureExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ExplicitClosure;
  }
  static bool classof(const DeclContext *DC) {
    return isa<CapturingExpr>(DC) && classof(cast<CapturingExpr>(DC));
  }
  static bool classof(const CapturingExpr *E) { return classof(cast<Expr>(E)); }
};
  
/// ImplicitClosureExpr - This is a closure of the contained subexpression that
/// is formed when an scalar expression is converted to [auto_closure] function
/// type.  For example:  
///   var x : [auto_closure] () -> int = 4
///
class ImplicitClosureExpr : public ClosureExpr {
public:
  ImplicitClosureExpr(Expr *Body, DeclContext *Parent, Type ResultTy)
    : ClosureExpr(ExprKind::ImplicitClosure, Body,
                  DeclContextKind::CapturingExpr, Parent, ResultTy) {}
  
  SourceRange getSourceRange() const { return getBody()->getSourceRange(); }

  ArrayRef<Pattern*> getParamPatterns() const {
    return ArrayRef<Pattern*>();
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const ImplicitClosureExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ImplicitClosure;
  }
  static bool classof(const DeclContext *DC) {
    return isa<CapturingExpr>(DC) && classof(cast<CapturingExpr>(DC));
  }
  static bool classof(const CapturingExpr *E) { return classof(cast<Expr>(E)); }
};


/// NewArrayExpr - The allocation of an array.  Allocates and constructs
/// the array, then injects that into the corresponding slice type.
class NewArrayExpr : public Expr {
public:
  struct Bound {
    Expr *Value;
    SourceRange Brackets;

    Bound() = default;
    Bound(Expr *value, SourceRange brackets)
      : Value(value), Brackets(brackets) {}
  };

private:
  Type ElementTy;
  unsigned NumBounds;
  SourceLoc NewLoc;
  Expr *InjectionFn;

  NewArrayExpr(SourceLoc newLoc, Type elementTy, unsigned numBounds, Type ty)
    : Expr(ExprKind::NewArray, ty), ElementTy(elementTy),
      NumBounds(numBounds), NewLoc(newLoc), InjectionFn(nullptr) {}

  Bound *getBoundsBuffer() {
    return reinterpret_cast<Bound*>(this + 1);
  }
  const Bound *getBoundsBuffer() const {
    return reinterpret_cast<const Bound*>(this + 1);
  }

public:
  static NewArrayExpr *create(ASTContext &Context, SourceLoc newLoc,
                              Type elementTy, ArrayRef<Bound> bounds);

  unsigned getNumBounds() const { return NumBounds; }

  MutableArrayRef<Bound> getBounds() {
    return MutableArrayRef<Bound>(getBoundsBuffer(), getNumBounds());
  }
  ArrayRef<Bound> getBounds() const {
    return ArrayRef<Bound>(getBoundsBuffer(), getNumBounds());
  }

  /// Return the location of the 'new' keyword.
  SourceLoc getNewLoc() const { return NewLoc; }

  SourceRange getSourceRange() const {
    return SourceRange(NewLoc, getBounds().back().Brackets.End);
  }
  SourceLoc getLoc() const { return NewLoc; }

  /// Set the injection function expression to use.
  void setInjectionFunction(Expr *fn) { InjectionFn = fn; }
  Expr *getInjectionFunction() const {
    assert(InjectionFn != nullptr);
    return InjectionFn;
  }

  Type getElementType() const { return ElementTy; }
  void setElementType(Type ty) { ElementTy = ty; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const NewArrayExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::NewArray;
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
  ApplyExpr(ExprKind Kind, Expr *Fn, Expr *Arg, Type Ty = Type())
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
  CallExpr(Expr *fn, Expr *arg, Type ty = Type())
    : ApplyExpr(ExprKind::Call, fn, arg, ty) {}

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
  UnaryExpr(Expr *Fn, Expr *Arg, Type Ty = Type())
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
  BinaryExpr(Expr *Fn, TupleExpr *Arg, Type Ty = Type())
    : ApplyExpr(ExprKind::Binary, Fn, Arg, Ty) {}

  SourceLoc getLoc() const { return getFn()->getLoc(); }
                 
  SourceRange getSourceRange() const {
    return getArg()->getSourceRange();
  }  

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BinaryExpr *) { return true; }
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Binary;}
};

/// ConstructorCallExpr - This is the application of an argument to a metatype,
/// which resolves to construction of the type.  For example, "SomeType(1,2,3)".
/// The function in the application is known to be one of the static methods to
/// construct the type.  This is formed by Sema, and is just a sugared form of
/// ApplyExpr.
class ConstructorCallExpr : public ApplyExpr {
public:
  ConstructorCallExpr(Expr *FnExpr, Expr *ArgExpr, Type Ty = Type())
    : ApplyExpr(ExprKind::ConstructorCall, FnExpr, ArgExpr, Ty) {
  }

  SourceRange getSourceRange() const {
    return SourceRange(getFn()->getStartLoc(), getArg()->getEndLoc());
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ConstructorCallExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ConstructorCall;
  }
};

/// DotSyntaxCallExpr - Refer to an element or method of a type, e.g. P.x.  'x'
/// is modeled as a DeclRefExpr or OverloadSetRefExpr on the field's decl.
///
class DotSyntaxCallExpr : public ApplyExpr {
  SourceLoc DotLoc;
  
public:
  DotSyntaxCallExpr(Expr *FnExpr, SourceLoc DotLoc, Expr *BaseExpr,
                    Type Ty = Type())
    : ApplyExpr(ExprKind::DotSyntaxCall, FnExpr, BaseExpr, Ty),
      DotLoc(DotLoc) {
  }

  SourceLoc getDotLoc() const { return DotLoc; }
  SourceLoc getLoc() const { return getArg()->getStartLoc(); }
  SourceLoc getEndLoc() const {
    return DotLoc.isValid()? getFn()->getEndLoc() : getArg()->getEndLoc();
  }
  
  SourceRange getSourceRange() const {
    // Implicit 'this' receivers don't have location info for DotLoc or the
    // 'arg' expression.
    if (DotLoc.isValid())
      return SourceRange(getArg()->getStartLoc(), getEndLoc());
    return getFn()->getSourceRange();
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DotSyntaxCallExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DotSyntaxCall;
  }
};

  
/// DotSyntaxBaseIgnoredExpr - When a.b resolves to something that does not need
/// the actual value of the base (e.g. when applied to a metatype, module, or
/// the base of a 'static' function) this expression node is created.  The
/// semantics are that its base is evaluated and discarded, then 'b' is
/// evaluated and returned as the result of the expression.
class DotSyntaxBaseIgnoredExpr : public Expr {
  Expr *LHS;
  SourceLoc DotLoc;
  Expr *RHS;
public:
  DotSyntaxBaseIgnoredExpr(Expr *LHS, SourceLoc DotLoc, Expr *RHS)
    : Expr(ExprKind::DotSyntaxBaseIgnored, RHS->getType()),
      LHS(LHS), DotLoc(DotLoc), RHS(RHS) {
  }
  
  Expr *getLHS() { return LHS; }
  void setLHS(Expr *E) { LHS = E; }
  SourceLoc getDotLoc() const { return DotLoc; }
  Expr *getRHS() { return RHS; }
  void setRHS(Expr *E) { RHS = E; }

  SourceLoc getStartLoc() const {
    return DotLoc.isValid()? LHS->getStartLoc() : RHS->getStartLoc();
  }
  
  SourceRange getSourceRange() const {
    return SourceRange(getStartLoc(), RHS->getEndLoc());
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const DotSyntaxBaseIgnoredExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DotSyntaxBaseIgnored;
  }
};

/// CoerceExpr - Represents a function application a(b) that is actually a
/// type coercion of the expression 'b' to the type 'a'. This expression
/// is not used for actual casts, because those are constructor calls.
class CoerceExpr : public Expr {
  Expr *LHS;
  Expr *RHS;
  
public:
  CoerceExpr(Expr *LHS, Expr *RHS)
    : Expr(ExprKind::Coerce, RHS->getType()), LHS(LHS), RHS(RHS) { }
           
  Expr *getLHS() const { return LHS; }
  Expr *getRHS() const { return RHS; }
  
  void setLHS(Expr *E) { LHS = E; }
  void setRHS(Expr *E) { RHS = E; }
  
  
  SourceLoc getStartLoc() const { return LHS->getStartLoc(); }
  SourceLoc getEndLoc() const { return RHS->getEndLoc(); }
  
  SourceRange getSourceRange() const {
    return SourceRange(LHS->getStartLoc(), RHS->getEndLoc());
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const CoerceExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Coerce;
  }
};
  
} // end namespace swift

#endif
