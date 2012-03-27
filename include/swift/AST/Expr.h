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

public:
  Expr(ExprKind Kind, Type Ty = Type()) : Kind(Kind), Ty(Ty) {}

  /// getKind - Return the kind of this expression.
  ExprKind getKind() const { return Kind; }

  /// getType - Return the type of this expression.
  Type getType() const { return Ty; }

  /// setType - Sets the type of this expression.
  void setType(Type T) { Ty = T; }

  /// setDependentType - Sets this expression to have the given
  /// dependent type.  This is just like setType except more
  /// self-documenting.
  void setDependentType(Type T) { setType(T); }

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
  
  /// ConversionRank - This enum specifies the rank of an implicit conversion
  /// of a value from one type to another.  These are ordered from cheapest to
  /// most expensive.
  enum ConversionRank {
    /// CR_Identity - It is free to convert these two types.  For example,
    /// identical types return this, types that are just aliases of each other
    /// do as well, conversion of a scalar to a single-element tuple, etc.
    CR_Identity,

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
  

/// IntegerLiteralExpr - Integer literal, like '4'.  After semantic analysis
/// assigns types, this is guaranteed to only have a BuiltinIntegerType.
class IntegerLiteralExpr : public Expr {
  StringRef Val;  // Use StringRef instead of APInt, APInt leaks.
  SourceLoc Loc;

public:
  IntegerLiteralExpr(StringRef Val, SourceLoc Loc)
    : Expr(ExprKind::IntegerLiteral), Val(Val), Loc(Loc) {}
  
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
class FloatLiteralExpr : public Expr {
  StringRef Val; // Use StringRef instead of APFloat, APFloat leaks.
  SourceLoc Loc;

public:
  FloatLiteralExpr(StringRef Val, SourceLoc Loc)
    : Expr(ExprKind::FloatLiteral), Val(Val), Loc(Loc) {}

  APFloat getValue() const;

  StringRef getText() const { return Val; }
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
class OverloadSetRefExpr : public Expr {
  ArrayRef<ValueDecl*> Decls;
  SourceLoc Loc;

public:
  OverloadSetRefExpr(ArrayRef<ValueDecl*> decls, SourceLoc Loc,
                     Type Ty = Type())
  : Expr(ExprKind::OverloadSetRef, Ty), Decls(decls), Loc(Loc) {}

  ArrayRef<ValueDecl*> getDecls() const { return Decls; }

  SourceLoc getLoc() const { return Loc; }
  SourceRange getSourceRange() const { return Loc; }
  
  /// createWithCopy - Create and return a new OverloadSetRefExpr or a new
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
  
/// TupleExpr - Parenthesized expressions like '(a=x+x)' and '(x, y, 4)'.  Tuple
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
  MutableArrayRef<Expr *> SubExprs;
  /// SubExprNames - Can be null if no names.  Otherwise length = SubExpr.size()
  Identifier *SubExprNames;
  SourceLoc RParenLoc;
  
public:
  TupleExpr(SourceLoc LParenLoc, MutableArrayRef<Expr *> SubExprs,
            Identifier *SubExprNames, SourceLoc RParenLoc,
            Type Ty = Type())
    : Expr(ExprKind::Tuple, Ty), LParenLoc(LParenLoc), SubExprs(SubExprs),
      SubExprNames(SubExprNames), RParenLoc(RParenLoc) {
    assert(LParenLoc.isValid() == RParenLoc.isValid() &&
           "Mismatched parenthesis location information validity");
  }

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  SourceRange getSourceRange() const;

  //unsigned getNumElements() const { return NumSubExprs; }

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

/// UnresolvedDotExpr - A field access (foo.bar) on an expression with dependent
/// type.
class UnresolvedDotExpr : public Expr {
  Expr *SubExpr;
  SourceLoc DotLoc;
  Identifier Name;
  SourceLoc NameLoc;
public:
  UnresolvedDotExpr(Expr *subexpr, SourceLoc dotloc, Identifier name,
                    SourceLoc nameloc)
  : Expr(ExprKind::UnresolvedDot), SubExpr(subexpr), DotLoc(dotloc),
    Name(name), NameLoc(nameloc) {}
  
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

public:
  CapturingExpr(ExprKind Kind, Type FnType, DeclContextKind DCKind,
                DeclContext *Parent)
    : Expr(Kind, FnType), DeclContext(DCKind, Parent) {}

  ArrayRef<ValueDecl*> getCaptures() { return Captures; }
  void setCaptures(ArrayRef<ValueDecl*> C) { Captures = C; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const CapturingExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() >= ExprKind::First_CapturingExpr &&
           E->getKind() <= ExprKind::Last_CapturingExpr;
  }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() >= DeclContextKind::First_Capturing &&
           DC->getContextKind() <= DeclContextKind::Last_Capturing;
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
    : CapturingExpr(ExprKind::Func, FnType, DeclContextKind::FuncExpr, Parent),
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
    return DC->getContextKind() == DeclContextKind::FuncExpr;
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
    return DC->getContextKind() >= DeclContextKind::First_Closure &&
           DC->getContextKind() <= DeclContextKind::Last_Closure;
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
                  DeclContextKind::ExplicitClosureExpr, Parent),
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
    return DC->getContextKind() == DeclContextKind::ExplicitClosureExpr;
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
                  DeclContextKind::ImplicitClosureExpr, Parent, ResultTy) {}
  
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
    return DC->getContextKind() == DeclContextKind::ImplicitClosureExpr;
  }
  static bool classof(const CapturingExpr *E) { return classof(cast<Expr>(E)); }
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
  static bool classof(const Expr *E) { return E->getKind() == ExprKind::Binary;}
};

/// ConstructorCallExpr - This is the application of an argument to a metatype,
/// which resolves to construction of the type.  For example, "SomeType(1,2,3)".
/// The function in the application is known to be one of the plus methods to
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
  
  SourceRange getSourceRange() const {
    return SourceRange(getArg()->getStartLoc(), getFn()->getEndLoc());
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DotSyntaxCallExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DotSyntaxCall;
  }
};

  
/// DotSyntaxBaseIgnoredExpr - When a.b resolves to something that does not need
/// the actual value of the base (e.g. when applied to a metatype, module, or
/// the base of a 'plus' function) this expression node is created.  The
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

  SourceRange getSourceRange() const {
    return SourceRange(LHS->getStartLoc(), RHS->getEndLoc());
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const DotSyntaxBaseIgnoredExpr *) { return true; }
  static bool classof(const Expr *E) {
    return E->getKind() == ExprKind::DotSyntaxBaseIgnored;
  }

};
  
} // end namespace swift

#endif
