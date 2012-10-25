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
#include "swift/AST/Substitution.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class ArchetypeType;
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
  class ProtocolConformance;
  class FuncDecl;
  class ConstructorDecl;
  class SubstitutableType;
  
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
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Error;
  }
};

/// LiteralExpr - Common base class between the literals.
class LiteralExpr : public Expr {
public:
  LiteralExpr(ExprKind Kind) : Expr(Kind) {}
  
  
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

  /// hasBaseObject - Determine whether this overloaded expression has a
  /// concrete base object (which is not a metatype).
  bool hasBaseObject() const;

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
  DeclRefKind RefKind;

public:
  UnresolvedDeclRefExpr(Identifier name, DeclRefKind refKind, SourceLoc loc)
    : Expr(ExprKind::UnresolvedDeclRef), Name(name), Loc(loc),
      RefKind(refKind) {
  }
  
  Identifier getName() const { return Name; }
  DeclRefKind getRefKind() const { return RefKind; }

  SourceRange getSourceRange() const { return Loc; }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::UnresolvedDeclRef;
  }
};

/// MemberRefExpr - This represents 'a.b' where we are referring to a member
/// of a type, such as a property or variable.
///
/// Note that methods found via 'dot' syntax are expressed as DotSyntaxCallExpr
/// nodes, because 'a.f' is actually an application of 'a' (the implicit object
/// argument) to the function 'f'.
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
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::MemberRef;
  }
};  

/// ExistentialMemberRefExpr - This represents 'a.b' where we are referring to
/// a member of an existential type (e.g., a protocol member).
///
/// \code
/// protocol Printable {
///   func print(format : Format)
/// }
///
/// var p : Printable
/// p.print // An ExistentialMemberRefExpr with type (format : Format) -> ()
/// \endcode
class ExistentialMemberRefExpr : public Expr {
  Expr *Base;
  ValueDecl *Value;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  
public:  
  ExistentialMemberRefExpr(Expr *Base, SourceLoc DotLoc, ValueDecl *Value,
                           SourceLoc NameLoc);
  Expr *getBase() const { return Base; }
  ValueDecl *getDecl() const { return Value; }
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getDotLoc() const { return DotLoc; }
  
  void setBase(Expr *E) { Base = E; }
  
  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const {
    if (Base->isImplicit())
      return SourceRange(NameLoc);
    
    return SourceRange(Base->getStartLoc(), NameLoc);
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ExistentialMemberRef;
  }
};

/// ArchetypeMemberRefExpr - This represents 'a.b' where we are referring to
/// a member of an archetype type (e.g., within a generic function).
///
/// \code
/// protocol Printable {
///   func print(format : Format)
/// }
///
/// func doPrint<P : Printable>(p : P) {
///   p.print(Format());
/// }
/// \endcode
class ArchetypeMemberRefExpr : public Expr {
  Expr *Base;
  ValueDecl *Value;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  
public:  
  ArchetypeMemberRefExpr(Expr *Base, SourceLoc DotLoc, ValueDecl *Value,
                         SourceLoc NameLoc);
  Expr *getBase() const { return Base; }
  ValueDecl *getDecl() const { return Value; }
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getDotLoc() const { return DotLoc; }
  
  void setBase(Expr *E) { Base = E; }
  
  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const {
    if (Base->isImplicit())
      return SourceRange(NameLoc);
    
    return SourceRange(Base->getStartLoc(), NameLoc);
  }

  /// getArchetype - Retrieve the archetype whose member is being accessed.
  ArchetypeType *getArchetype() const;

  /// isBaseIgnored - Determine whether the base expression is actually ignored,
  /// rather than being used as, e.g.,  the 'this' argument passed to an
  /// instance method or the base of a variable access.
  bool isBaseIgnored() const;

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ArchetypeMemberRef;
  }
};

class GenericMemberRefExpr : public Expr {
  Expr *Base;
  ValueDecl *Value;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  ArrayRef<Substitution> Substitutions;

public:  
  GenericMemberRefExpr(Expr *Base, SourceLoc DotLoc, ValueDecl *Value,
                       SourceLoc NameLoc);

  Expr *getBase() const { return Base; }
  ValueDecl *getDecl() const { return Value; }
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getDotLoc() const { return DotLoc; }

  /// \brief Retrieve the set of substitutions applied to specialize the
  /// member.
  ///
  /// Each substitution contains the archetype being substitued, the type it is
  /// being replaced with, and the protocol conformance relationships.
  ArrayRef<Substitution> getSubstitutions() const { return Substitutions; }

  void setSubstitutions(ArrayRef<Substitution> S) { Substitutions = S; }
  
  void setBase(Expr *E) { Base = E; }

  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const {
    if (Base->isImplicit())
      return SourceRange(NameLoc);

    return SourceRange(Base->getStartLoc(), NameLoc);
  }

  /// isBaseIgnored - Determine whether the base expression is actually
  /// ignored, rather than being used as, e.g., the 'this' argument passed
  /// to an instance method or the base of a variable access.
  bool isBaseIgnored() const;

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::GenericMemberRef;
  }
};

/// UnresolvedMemberExpr - This represents '.foo', an unresolved reference to a
/// member, which is to be resolved with context sensitive type information into
/// bar.foo.  These always have unresolved type.
class UnresolvedMemberExpr : public Expr {
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  Identifier Name;

public:  
  UnresolvedMemberExpr(SourceLoc dotLoc, SourceLoc nameLoc,
                       Identifier name)
    : Expr(ExprKind::UnresolvedMember),
      DotLoc(dotLoc), NameLoc(nameLoc), Name(name) {
  }

  Identifier getName() const { return Name; }
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getDotLoc() const { return DotLoc; }

  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const { 
    return SourceRange(DotLoc, NameLoc);
  }
  
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
    assert(LParenLoc.isValid() == RParenLoc.isValid() &&
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
  
  /// getIndex - Retrieve the index of the subscript expression, i.e., the
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
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Subscript;
  }
};

/// ExistentialSubscriptExpr - Subscripting expressions like a[i] that refer to
/// an element within a container, where the container has existential type.
///
/// There is no built-in subscripting in the language. Rather, a fully
/// type-checked and well-formed subscript expression refers to a subscript
/// declaration, which provides a getter and (optionally) a setter that will
/// be used to perform reads/writes.
class ExistentialSubscriptExpr : public Expr {
  SubscriptDecl *D;
  SourceRange Brackets;
  Expr *Base;
  Expr *Index;
  
public:
  ExistentialSubscriptExpr(Expr *Base, SourceLoc LBracketLoc, Expr *Index,
                           SourceLoc RBracketLoc, SubscriptDecl *D);
  
  /// getBase - Retrieve the base of the subscript expression, i.e., the
  /// value being indexed. This value has existential type.
  Expr *getBase() const { return Base; }
  void setBase(Expr *E) { Base = E; }
  
  /// getIndex - Retrieve the index of the subscript expression, i.e., the
  /// "offset" into the base value.
  Expr *getIndex() const { return Index; }
  void setIndex(Expr *E) { Index = E; }
  
  /// getDecl - Retrieve the subscript declaration that this subscripting
  /// operation refers to. 
  SubscriptDecl *getDecl() const { return D; }
  void setDecl(SubscriptDecl *D) { this->D = D; }
  
  SourceLoc getLBracketLoc() const { return Brackets.Start; }
  SourceLoc getRBracketLoc() const { return Brackets.End; }
  
  SourceRange getSourceRange() const {
    return SourceRange(Base->getStartLoc(), Brackets.End);
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ExistentialSubscript;
  }
};

/// ArchetypeSubscriptExpr - Subscripting expressions like a[i] that refer to
/// an element within a container, where the container is an archetype.
class ArchetypeSubscriptExpr : public Expr {
  SubscriptDecl *D;
  SourceRange Brackets;
  Expr *Base;
  Expr *Index;
  
public:
  ArchetypeSubscriptExpr(Expr *Base, SourceLoc LBracketLoc, Expr *Index,
                         SourceLoc RBracketLoc, SubscriptDecl *D);
  
  /// getBase - Retrieve the base of the subscript expression, i.e., the
  /// value being indexed. This value has archetype type.
  Expr *getBase() const { return Base; }
  void setBase(Expr *E) { Base = E; }
  
  /// getIndex - Retrieve the index of the subscript expression, i.e., the
  /// "offset" into the base value.
  Expr *getIndex() const { return Index; }
  void setIndex(Expr *E) { Index = E; }
  
  /// getDecl - Retrieve the subscript declaration that this subscripting
  /// operation refers to. 
  SubscriptDecl *getDecl() const { return D; }
  void setDecl(SubscriptDecl *D) { this->D = D; }
  
  SourceLoc getLBracketLoc() const { return Brackets.Start; }
  SourceLoc getRBracketLoc() const { return Brackets.End; }
  
  SourceRange getSourceRange() const {
    return SourceRange(Base->getStartLoc(), Brackets.End);
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ArchetypeSubscript;
  }
};

/// GenericSubscriptExpr - Subscripting expressions like a[i] that refer to
/// an element within a container, where the container is a generic type.
class GenericSubscriptExpr : public Expr {
  SubscriptDecl *D;
  SourceRange Brackets;
  Expr *Base;
  Expr *Index;
  ArrayRef<Substitution> Substitutions;

public:
  GenericSubscriptExpr(Expr *Base, SourceLoc LBracketLoc, Expr *Index,
                       SourceLoc RBracketLoc, SubscriptDecl *D);
  
  /// getBase - Retrieve the base of the subscript expression, i.e., the
  /// value being indexed. This value has generic type.
  Expr *getBase() const { return Base; }
  void setBase(Expr *E) { Base = E; }
  
  /// getIndex - Retrieve the index of the subscript expression, i.e., the
  /// "offset" into the base value.
  Expr *getIndex() const { return Index; }
  void setIndex(Expr *E) { Index = E; }
  
  /// getDecl - Retrieve the subscript declaration that this subscripting
  /// operation refers to. 
  SubscriptDecl *getDecl() const { return D; }
  void setDecl(SubscriptDecl *D) { this->D = D; }

  /// \brief Retrieve the set of substitutions applied to specialize the
  /// member.
  ///
  /// Each substitution contains the archetype being substitued, the type it is
  /// being replaced with, and the protocol conformance relationships.
  ArrayRef<Substitution> getSubstitutions() const { return Substitutions; }

  void setSubstitutions(ArrayRef<Substitution> S) { Substitutions = S; }

  SourceLoc getLBracketLoc() const { return Brackets.Start; }
  SourceLoc getRBracketLoc() const { return Brackets.End; }
  
  SourceRange getSourceRange() const {
    return SourceRange(Base->getStartLoc(), Brackets.End);
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::GenericSubscript;
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
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::OverloadedSubscript;
  }
};

/// UnresolvedDotExpr - A field access (foo.bar) on an expression with
/// unresolved type.
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
    if (DotLoc.isInvalid())
      return SourceRange(NameLoc, NameLoc);
    return SourceRange(SubExpr->getStartLoc(), NameLoc);
  }
  
  SourceLoc getDotLoc() const { return DotLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *e) { SubExpr = e; }

  Identifier getName() const { return Name; }
  SourceLoc getNameLoc() const { return NameLoc; }

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
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Module;
  }
};


/// TupleElementExpr - Refer to an element of a tuple,
/// e.g. "(1,field=2).field".
class TupleElementExpr : public Expr {
  Expr *SubExpr;
  SourceLoc NameLoc;
  unsigned FieldNo;
  SourceLoc DotLoc;

public:
  TupleElementExpr(Expr *SubExpr, SourceLoc DotLoc, unsigned FieldNo,
                   SourceLoc NameLoc, Type Ty)
    : Expr(ExprKind::TupleElement, Ty), SubExpr(SubExpr),
      NameLoc(NameLoc), FieldNo(FieldNo), DotLoc(DotLoc) {}

  SourceLoc getLoc() const { return NameLoc; }
  Expr *getBase() const { return SubExpr; }
  void setBase(Expr *e) { SubExpr = e; }

  unsigned getFieldNumber() const { return FieldNo; }
  SourceLoc getNameLoc() const { return NameLoc; }  

  SourceRange getSourceRange() const { 
    return SourceRange(getBase()->getStartLoc(), getNameLoc());
  }

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::TupleElement;
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

  /// If we're doing a varargs shuffle, this is the function to build the
  /// destination slice type.
  Expr *InjectionFn;

public:
  TupleShuffleExpr(Expr *subExpr, ArrayRef<int> elementMapping, Type ty)
    : ImplicitConversionExpr(ExprKind::TupleShuffle, subExpr, ty),
      ElementMapping(elementMapping), InjectionFn(nullptr) {}

  ArrayRef<int> getElementMapping() const { return ElementMapping; }

  /// Set the injection function expression to use.
  void setVarargsInjectionFunction(Expr *fn) { InjectionFn = fn; }
  Expr *getVarargsInjectionFunction() const {
    assert(InjectionFn != nullptr);
    return InjectionFn;
  }
  Expr *getVarargsInjectionFunctionOrNull() const {
    return InjectionFn;
  }

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

  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Load; }
};

/// MaterializeExpr - Turn an r-value into an l-value by placing it in
/// temporary memory.
class MaterializeExpr : public ImplicitConversionExpr {
public:
  MaterializeExpr(Expr *subExpr, Type ty)
    : ImplicitConversionExpr(ExprKind::Materialize, subExpr, ty) {}

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

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Requalify;
  }
};
  
/// FunctionConversionExpr - Convert a function to another function type,
/// which might involve renaming the parameters or handling substitutions
/// of subtypes (in the return) or supertypes (in the input).
///
/// FIXME: This should be a CapturingExpr.
class FunctionConversionExpr : public ImplicitConversionExpr {
  // FIXME: Sink into Expr.
  unsigned IsTrivial : 1;
public:
  FunctionConversionExpr(Expr *subExpr, Type type, bool IsTrivial)
    : ImplicitConversionExpr(ExprKind::FunctionConversion, subExpr, type),
      IsTrivial(IsTrivial) {}
  
  /// \brief Whether this is a "trivial" conversion, that only includes
  /// parameter renaming and other similarly trivial operations that do not
  /// change the representation of the function.
  ///
  /// The 'trivial' computation is a "best effort" computation based on the
  /// language model itself. It does not account for conversions that may be
  /// made trivial if the layout of specific data types is known.
  bool isTrivial() const { return IsTrivial; }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::FunctionConversion;
  }
};

/// MetatypeConversionExpr - Convert a metatype to another metatype
/// using essentially a derived-to-base conversion.
class MetatypeConversionExpr : public ImplicitConversionExpr {
public:
  MetatypeConversionExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::MetatypeConversion, subExpr, type) {}
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::MetatypeConversion;
  }
};

/// ErasureExpr - Perform type erasure by converting a value to existential
/// type. For example:
///
/// \code
/// protocol Printable {
///   func print()
/// }
///
/// struct Book {
///   func print() { ... }
/// }
///
/// var printable : Printable = Book() // erases type
/// \endcode
class ErasureExpr : public ImplicitConversionExpr {
  ArrayRef<ProtocolConformance *> Conformances;
  
public:
  ErasureExpr(Expr *SubExpr, Type Ty,
              ArrayRef<ProtocolConformance *> Conformances)
    : ImplicitConversionExpr(ExprKind::Erasure, SubExpr, Ty),
      Conformances(Conformances) {}
  
  /// \brief Retrieve the mapping specifying how the type of the subexpression
  /// maps to the resulting existential type. If the resulting existential
  /// type involves several different protocols, there will be mappings for each
  /// of those protocols, in the order in which the existential type expands
  /// its properties.
  ///
  /// The entries in this array may be null, indicating that the conformance
  /// to the corresponding protocol is trivial (because the source
  /// type is either an archetype or an existential type that conforms to
  /// that corresponding protocol).
  ArrayRef<ProtocolConformance *> getConformances() const {
    return Conformances;
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Erasure;
  }
};

/// SpecializeExpr - Specializes a reference to a generic entity by binding
/// each of its type parameters to a specific type.
///
/// In a type-checked AST, every reference to a generic entity will be bound
/// (at some point) by a SpecializeExpr. The type of a SpecializeExpr is the
/// type of the entity with all of the type parameters substituted.
///
/// An example:
/// \code
/// func identity<T>(x : T) -> T { return x }
///
/// var i : Int = identity(17) // 'identity' is specialized to (x : Int) -> Int
/// \endcode
class SpecializeExpr : public ImplicitConversionExpr {
public:
  typedef swift::Substitution Substitution;

private:
  ArrayRef<Substitution> Substitutions;

public:
  SpecializeExpr(Expr *SubExpr, Type Ty, ArrayRef<Substitution> Substitutions)
    : ImplicitConversionExpr(ExprKind::Specialize, SubExpr, Ty),
      Substitutions(Substitutions) { }

  /// \brief Retrieve the set of substitutions applied to specialize the
  /// subexpression.
  ///
  /// Each substitution contains the archetype being substitued, the type it is
  /// being replaced with, and the protocol conformance relationships.
  ArrayRef<Substitution> getSubstitutions() const { return Substitutions; }

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Specialize;
  }
};

/// GetMetatypeExpr - Given a value of type T, returns the corresponding value
/// of type T.metatype.
class GetMetatypeExpr : public ImplicitConversionExpr {
public:
  GetMetatypeExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::GetMetatype, subExpr, type) {}

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::GetMetatype;
  }
};

/// GetMetatypeExpr - Convert a value of class type to a value of base class
/// type.
class DerivedToBaseExpr : public ImplicitConversionExpr {
public:
  DerivedToBaseExpr(Expr *subExpr, Type type)
    : ImplicitConversionExpr(ExprKind::DerivedToBase, subExpr, type) {}

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DerivedToBase;
  }
};

/// ScalarToTupleExpr - Convert a scalar to tuple type.
class ScalarToTupleExpr : public ImplicitConversionExpr {
  unsigned ScalarField;

  /// If we're doing a varargs shuffle, this is the function to build the
  /// destination slice type.
  Expr *InjectionFn;

public:
  ScalarToTupleExpr(Expr *subExpr, Type type, unsigned ScalarField,
                    Expr *InjectionFn = nullptr)
    : ImplicitConversionExpr(ExprKind::ScalarToTuple, subExpr, type),
      ScalarField(ScalarField), InjectionFn(InjectionFn) {}

  unsigned getScalarField() { return ScalarField; }

  Expr *getVarargsInjectionFunction() { return InjectionFn; }

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
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Sequence;
  }
};

/// CapturingExpr - a FuncExpr or a ClosureExpr; always returns something
/// of function type, and can capture variables from an enclosing scope.
class CapturingExpr : public Expr, public DeclContext {
  ArrayRef<ValueDecl*> Captures;
  bool IsNotCaptured;

public:
  CapturingExpr(ExprKind Kind, Type FnType, DeclContext *Parent)
    : Expr(Kind, FnType), DeclContext(DeclContextKind::CapturingExpr, Parent),
      IsNotCaptured(false) {}

  ArrayRef<ValueDecl*> getCaptures() { return Captures; }
  void setCaptures(ArrayRef<ValueDecl*> C) { Captures = C; }

  bool isNotCaptured() { return IsNotCaptured; }
  void setIsNotCaptured(bool v) { IsNotCaptured = v; }

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

  FuncDecl *TheFuncDecl;

  TypeLoc FnRetType;

  Pattern **getParamsBuffer() {
    return reinterpret_cast<Pattern**>(this+1);
  }
  Pattern * const *getParamsBuffer() const {
    return reinterpret_cast<Pattern*const*>(this+1);
  }
  
  FuncExpr(SourceLoc FuncLoc, unsigned NumPatterns, TypeLoc FnRetType,
           BraceStmt *Body, DeclContext *Parent)
    : CapturingExpr(ExprKind::Func, Type(), Parent),
      FuncLoc(FuncLoc), NumPatterns(NumPatterns), Body(Body),
      TheFuncDecl(nullptr), FnRetType(FnRetType) {}
public:
  static FuncExpr *create(ASTContext &Context, SourceLoc FuncLoc,
                          ArrayRef<Pattern*> Params, TypeLoc FnRetType,
                          BraceStmt *Body, DeclContext *Parent);

  SourceRange getSourceRange() const;
  SourceLoc getLoc() const { return FuncLoc; }

  ArrayRef<Pattern*> getParamPatterns() const {
    return ArrayRef<Pattern*>(getParamsBuffer(), NumPatterns);
  }

  /// getNaturalArgumentCount - Returns the "natural" number of
  /// argument clauses taken by this function.  See the comment on
  /// FuncDecl.
  unsigned getNaturalArgumentCount() const {
    return NumPatterns;
  }
  
  FuncDecl *getDecl() const { return TheFuncDecl; }
  void setDecl(FuncDecl *f) { TheFuncDecl = f; }

  /// Returns the location of the 'func' keyword.
  SourceLoc getFuncLoc() const { return FuncLoc; }

  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *S) { Body = S; }

  TypeLoc &getBodyResultTypeLoc() { return FnRetType; }

  /// \brief Retrieve the result type of this function.
  Type getResultType(ASTContext &Ctx) const;

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
  ClosureExpr(ExprKind Kind, Expr *Body, DeclContext *Parent,
              Type ResultTy = Type())
    : CapturingExpr(Kind, ResultTy, Parent), Body(Body), Pat(0) {}

  Expr *getBody() const { return Body; }
  void setBody(Expr *e) { Body = e; }

  Pattern *getPattern() { return Pat; }
  const Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *pat) { Pat = pat; }
  ArrayRef<Pattern*> getParamPatterns() const {
    return Pat;
  }

  // Implement isa/cast/dyncast/etc.
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
/// e.g. "{ foo() }" or "{}".  This may contain AnonClosureArgExprs within it
/// that reference the formal arguments of the closure.
class ExplicitClosureExpr : public ClosureExpr {
  SourceLoc LBraceLoc, RBraceLoc;

  ArrayRef<VarDecl*> ParserVarDecls;

public:
  ExplicitClosureExpr(SourceLoc LBraceLoc, DeclContext *Parent,
                      Expr *Body = 0, SourceLoc RBraceLoc = SourceLoc())
    : ClosureExpr(ExprKind::ExplicitClosure, Body, Parent),
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
    : ClosureExpr(ExprKind::ImplicitClosure, Body, Parent, ResultTy) {}
  
  SourceRange getSourceRange() const { return getBody()->getSourceRange(); }

  ArrayRef<Pattern*> getParamPatterns() const {
    return ArrayRef<Pattern*>();
  }

  // Implement isa/cast/dyncast/etc.
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
  TypeLoc ElementTyAsWritten;
  Type ElementTy;
  unsigned NumBounds;
  SourceLoc NewLoc;
  Expr *InjectionFn;

  NewArrayExpr(SourceLoc newLoc, TypeLoc elementTy,
               unsigned numBounds)
    : Expr(ExprKind::NewArray, Type()), ElementTyAsWritten(elementTy),
      NumBounds(numBounds), NewLoc(newLoc), InjectionFn(nullptr) {}

  Bound *getBoundsBuffer() {
    return reinterpret_cast<Bound*>(this + 1);
  }
  const Bound *getBoundsBuffer() const {
    return reinterpret_cast<const Bound*>(this + 1);
  }

public:
  static NewArrayExpr *create(ASTContext &Context, SourceLoc newLoc,
                              TypeLoc elementTy, ArrayRef<Bound> bounds);

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

  bool hasInjectionFunction() const { return InjectionFn != nullptr; }

  bool hasElementType() const { return !ElementTy.isNull(); }

  Type getElementType() const {
    assert(ElementTy && "Element type not yet computed!");
    return ElementTy;
  }
  void setElementType(Type T) { ElementTy = T; }
  TypeLoc &getElementTypeLoc() { return ElementTyAsWritten; }

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::NewArray;
  }
};

/// MetatypeExpr - Evaluates an (optional) expression and produces a
/// metatype value.  If there's no base expression, this isn't really
/// a parsed form.
class MetatypeExpr : public Expr {
  Expr *Base;
  SourceLoc MetatypeLoc;

public:
  explicit MetatypeExpr(Expr *base, SourceLoc metatypeLoc, Type ty)
    : Expr(ExprKind::Metatype, ty), Base(base), MetatypeLoc(metatypeLoc) { }

  Expr *getBase() const { return Base; }
  void setBase(Expr *base) { Base = base; }

  SourceLoc getLoc() const { return MetatypeLoc; }

  SourceRange getSourceRange() const {
    if (Base) return SourceRange(Base->getStartLoc(), MetatypeLoc);
    return SourceRange(MetatypeLoc);
  }

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Metatype;
  }
};

/// OpaqueValueExpr - An expression referring to an opaque object of a
/// fixed type. It is used internally to perform type-checking when we require
/// an expression but do not want to form a complete expression.
class OpaqueValueExpr : public Expr {
  SourceLoc Loc;
  
public:
  explicit OpaqueValueExpr(SourceLoc Loc, Type Ty)
    : Expr(ExprKind::OpaqueValue, Ty), Loc(Loc) { }
  
  SourceRange getSourceRange() const { return Loc; }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::OpaqueValue; 
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
  void setArg(Expr *e) {
    assert((getKind() != ExprKind::Binary || isa<TupleExpr>(e)) &&
           "BinaryExprs must have a TupleExpr as the argument");
    Arg = e;
  }

  ValueDecl *getCalledValue() const;

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
  
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Call; }
};
  
/// PrefixUnaryExpr - Prefix unary expressions like '!y'.
class PrefixUnaryExpr : public ApplyExpr {
public:
  PrefixUnaryExpr(Expr *Fn, Expr *Arg, Type Ty = Type())
    : ApplyExpr(ExprKind::PrefixUnary, Fn, Arg, Ty) {}

  SourceLoc getLoc() const { return getFn()->getStartLoc(); }
  
  SourceRange getSourceRange() const {
    return SourceRange(getFn()->getStartLoc(), getArg()->getEndLoc()); 
  }  

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::PrefixUnary;
  }
};

/// PostfixUnaryExpr - Prefix unary expressions like '!y'.
class PostfixUnaryExpr : public ApplyExpr {
public:
  PostfixUnaryExpr(Expr *Fn, Expr *Arg, Type Ty = Type())
    : ApplyExpr(ExprKind::PostfixUnary, Fn, Arg, Ty) {}

  SourceLoc getLoc() const { return getFn()->getStartLoc(); }
  
  SourceRange getSourceRange() const {
    return SourceRange(getArg()->getStartLoc(), getFn()->getEndLoc()); 
  }  

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::PostfixUnary;
  }
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

  TupleExpr *getArg() const { return cast<TupleExpr>(ApplyExpr::getArg()); }

  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Binary;}
};

/// NewReferenceExpr - The allocation of a reference type.  Allocates and
/// constructs an object, then returns a reference to it.
class NewReferenceExpr : public ApplyExpr {
private:
  SourceLoc NewLoc;
  TypeLoc ElementTy;

public:
  NewReferenceExpr(TypeLoc ty, SourceLoc newLoc, Expr *CtorArg)
    : ApplyExpr(ExprKind::NewReference, nullptr, CtorArg), NewLoc(newLoc),
      ElementTy(ty) {}

  /// Return the location of the 'new' keyword.
  SourceLoc getNewLoc() const { return NewLoc; }

  SourceRange getSourceRange() const;
  SourceLoc getLoc() const { return NewLoc; }

  TypeLoc &getElementTypeLoc() { return ElementTy; }

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::NewReference;
  }
};

/// ThisApplyExpr - Abstract application that provides the 'this' pointer for
/// a method curried as (this : This) -> (params) -> result.
///
/// The application of a curried method to 'this' semantically differs from
/// normal function application because the 'this' parameter can be implicitly
/// materialized from an rvalue.
class ThisApplyExpr : public ApplyExpr {
protected:
  ThisApplyExpr(ExprKind K, Expr *FnExpr, Expr *BaseExpr, Type Ty)
    : ApplyExpr(K, FnExpr, BaseExpr, Ty) { }
  
public:
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_ThisApplyExpr &&
           E->getKind() <= ExprKind::Last_ThisApplyExpr;
  }
};

/// DotSyntaxCallExpr - Refer to a method of a type, e.g. P.x.  'x'
/// is modeled as a DeclRefExpr or OverloadSetRefExpr on the method.
class DotSyntaxCallExpr : public ThisApplyExpr {
  SourceLoc DotLoc;
  
public:
  DotSyntaxCallExpr(Expr *FnExpr, SourceLoc DotLoc, Expr *BaseExpr,
                    Type Ty = Type())
    : ThisApplyExpr(ExprKind::DotSyntaxCall, FnExpr, BaseExpr, Ty),
      DotLoc(DotLoc) {
  }

  SourceLoc getDotLoc() const { return DotLoc; }
  SourceLoc getLoc() const {
    return DotLoc.isValid() ? getArg()->getStartLoc() : getFn()->getStartLoc();
  }
  SourceLoc getEndLoc() const {
    return getFn()->getEndLoc();
  }
  
  SourceRange getSourceRange() const {
    // Implicit 'this' receivers don't have location info for DotLoc or the
    // 'arg' expression.
    if (DotLoc.isValid())
      return SourceRange(getArg()->getStartLoc(), getEndLoc());
    return getFn()->getSourceRange();
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DotSyntaxCall;
  }
};

/// ConstructorRefCallExpr - Refer to a constructor for a type P.  The
/// actual reference to function which returns the constructor is modeled
/// as a DeclRefExpr.
class ConstructorRefCallExpr : public ThisApplyExpr {
public:
  ConstructorRefCallExpr(Expr *FnExpr, Expr *BaseExpr, Type Ty = Type())
    : ThisApplyExpr(ExprKind::ConstructorRefCall, FnExpr, BaseExpr, Ty) {}

  SourceLoc getLoc() const {
    return getArg()->getLoc();
  }
  SourceRange getSourceRange() const {
    return SourceRange(getArg()->getSourceRange());
  }
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::ConstructorRefCall;
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

  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DotSyntaxBaseIgnored;
  }
};

/// CoerceExpr - Represents a function application a(b) that is actually a
/// type coercion of the expression 'b' to the type 'a'. This expression
/// is not used for actual casts, because those are constructor calls.
///
/// "a" is always a DeclRefExpr for a TypeDecl, e.g. "Int64" in "Int64(2)"
///
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
  
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::Coerce;
  }
};

} // end namespace swift

#endif
