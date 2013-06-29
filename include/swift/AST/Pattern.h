//===--- Pattern.h - Swift Language Pattern-Matching ASTs -------*- C++ -*-===//
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
// This file defines the Pattern class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PATTERN_H
#define SWIFT_PATTERN_H

#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"

namespace swift {
  class ASTContext;
  class ExprHandle;

/// PatternKind - The classification of different kinds of
/// value-matching pattern.
enum class PatternKind : uint8_t {
#define PATTERN(ID, PARENT) ID,
#include "PatternNodes.def"
};

/// Diagnostic printing of PatternKinds.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, PatternKind kind);
  
/// Pattern - Base class for all patterns in Swift.
class alignas(8) Pattern {
  class PatternBitfields {
    friend class Pattern;
    unsigned Kind : 8;
    unsigned isImplicit : 1;
  };
  enum { NumPatternBits = 9 };
  enum { NumBitsAllocated = 32 };

  class TuplePatternBitfields {
    friend class TuplePattern;
    unsigned : NumPatternBits;
    unsigned NumFields : NumBitsAllocated - NumPatternBits;
  };

protected:
  union {
    PatternBitfields PatternBits;
    TuplePatternBitfields TuplePatternBits;
  };

  Pattern(PatternKind kind) {
    PatternBits.Kind = unsigned(kind);
    PatternBits.isImplicit = false;
  }

private:
  /// The checked type of the pattern.
  Type Ty;

public:
  PatternKind getKind() const { return PatternKind(PatternBits.Kind); }

  bool isImplicit() const { return PatternBits.isImplicit; }
  void setImplicit() { PatternBits.isImplicit = true; }

  Pattern *getSemanticsProvidingPattern();
  const Pattern *getSemanticsProvidingPattern() const {
    return const_cast<Pattern*>(this)->getSemanticsProvidingPattern();
  }

  /// Returns whether this pattern has been type-checked yet.
  bool hasType() const { return !Ty.isNull(); }

  /// If thie pattern has been type-checked, return the type it
  /// matches.
  Type getType() const { assert(hasType()); return Ty; }

  /// Set the type of this pattern, given that it was previously not
  /// type-checked.
  void setType(Type ty);

  /// Overwrite the type of this pattern.
  void overwriteType(Type ty) { assert(hasType()); Ty = ty; }

  /// Returns the name directly bound by this pattern, or the null
  /// identifier if the pattern does not bind a name directly.
  Identifier getBoundName() const;

  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceLoc getLoc() const;

  /// \brief Collect the set of variables referenced in the given pattern.
  void collectVariables(SmallVectorImpl<VarDecl *> &variables) const;

  Pattern *clone(ASTContext &context) const;
  
  static bool classof(const Pattern *P) { return true; }
  
  //*** Allocation Routines ************************************************/

  void *operator new(size_t bytes, ASTContext &C);

  // Make placement new and vanilla new/delete illegal for Patterns.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;
  void *operator new(size_t bytes, void *data) = delete;
  
  void print(llvm::raw_ostream &OS) const;
  void dump() const;
  
  /// walk - This recursively walks the AST rooted at this pattern.
  Pattern *walk(ASTWalker &walker);
  Pattern *walk(ASTWalker &&walker) { return walk(walker); }
};

/// A pattern consisting solely of grouping parentheses around a
/// different pattern.
class ParenPattern : public Pattern {
  SourceLoc LPLoc, RPLoc;
  Pattern *SubPattern;
public:
  ParenPattern(SourceLoc lp, Pattern *sub, SourceLoc rp)
    : Pattern(PatternKind::Paren),
      LPLoc(lp), RPLoc(rp), SubPattern(sub) {
    assert(lp.isValid() == rp.isValid());
    if (!lp.isValid())
      setImplicit();
  }

  Pattern *getSubPattern() { return SubPattern; }
  const Pattern *getSubPattern() const { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const { return SourceRange(LPLoc, RPLoc); }
  SourceLoc getLoc() const { return SubPattern->getLoc(); }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Paren;
  }
};

/// An element of a tuple pattern.
class TuplePatternElt {
  Pattern *ThePattern;

  // FIXME: Init and VarargBaseType can be collapsed into one field.
  ExprHandle *Init;
  Type VarargBaseType;

public:
  TuplePatternElt() = default;
  explicit TuplePatternElt(Pattern *P, ExprHandle *init = nullptr,
                           Type varargBaseType = Type())
    : ThePattern(P), Init(init), VarargBaseType(varargBaseType) {}

  Pattern *getPattern() { return ThePattern; }
  const Pattern *getPattern() const { return ThePattern; }
  void setPattern(Pattern *p) { ThePattern = p; }

  ExprHandle *getInit() const { return Init; }

  bool isVararg() const { return !VarargBaseType.isNull(); }
  Type getVarargBaseType() const { return VarargBaseType; }
  void setVarargBaseType(Type ty) { VarargBaseType = ty; }

  /// \brief Revert this variadic tuple pattern element to a
  /// non-variadic version.
  ///
  /// Used for error recovery, when we've detected that the element
  /// is not the last one.
  void revertToNonVariadic();
};

/// A pattern consisting of a tuple of patterns.
class TuplePattern : public Pattern {
  SourceLoc LPLoc, RPLoc;
  // TuplePatternBits.NumFields

  TuplePatternElt *getFieldsBuffer() {
    return reinterpret_cast<TuplePatternElt *>(this+1);
  }
  const TuplePatternElt *getFieldsBuffer() const {
    return reinterpret_cast<const TuplePatternElt *>(this + 1);
  }

  TuplePattern(SourceLoc lp, unsigned numFields, SourceLoc rp)
      : Pattern(PatternKind::Tuple), LPLoc(lp), RPLoc(rp) {
    TuplePatternBits.NumFields = numFields;
    assert(lp.isValid() == rp.isValid());
    if (!lp.isValid())
      setImplicit();
  }

public:
  static TuplePattern *create(ASTContext &C, SourceLoc lp,
                              ArrayRef<TuplePatternElt> elements,
                              SourceLoc rp);

  /// \brief Create either a tuple pattern or a paren pattern, depending
  /// on the elements.
  static Pattern *createSimple(ASTContext &C, SourceLoc lp,
                               ArrayRef<TuplePatternElt> elements,
                               SourceLoc rp);

  unsigned getNumFields() const {
    return TuplePatternBits.NumFields;
  }

  MutableArrayRef<TuplePatternElt> getFields() {
    return MutableArrayRef<TuplePatternElt>(getFieldsBuffer(),
                                            getNumFields());
  }
  ArrayRef<TuplePatternElt> getFields() const {
    return ArrayRef<TuplePatternElt>(getFieldsBuffer(), getNumFields());
  }

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const { return SourceRange(LPLoc, RPLoc); }
  SourceLoc getLoc() const { return LPLoc; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Tuple;
  }
};

/// A pattern which binds a name to an arbitrary value of its type.
class NamedPattern : public Pattern {
  VarDecl *const Var;

public:
  explicit NamedPattern(VarDecl *Var) : Pattern(PatternKind::Named), Var(Var) {
    if (!Var->getLoc().isValid())
      setImplicit();
  }

  VarDecl *getDecl() const { return Var; }
  Identifier getBoundName() const { return Var->getName(); }

  SourceLoc getLoc() const { return Var->getLoc(); }
  SourceRange getSourceRange() const { return getLoc(); }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Named;
  }
};

/// A pattern which matches an arbitrary value of a type, but does not
/// bind a name to it.  This is spelled "_".
class AnyPattern : public Pattern {
  SourceLoc Loc;

public:
  AnyPattern(SourceLoc Loc) : Pattern(PatternKind::Any), Loc(Loc) {
    if (!Loc.isValid())
      setImplicit();
  }

  SourceLoc getLoc() const { return Loc; }
  SourceRange getSourceRange() const { return Loc; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Any;
  }
};

/// A pattern which matches a sub-pattern and annotates it with a
/// type. It is a compile-time error if the pattern does not statically match
/// a value of the type. This is different from IsaPattern, which is a refutable
/// dynamic type match.
class TypedPattern : public Pattern {
  Pattern *SubPattern;
  TypeLoc PatType;

public:
  TypedPattern(Pattern *pattern, TypeLoc tl)
    : Pattern(PatternKind::Typed), SubPattern(pattern), PatType(tl) {
    assert(pattern->isImplicit() == !tl.hasLocation());
    if (pattern->isImplicit())
      setImplicit();
  }

  Pattern *getSubPattern() { return SubPattern; }
  const Pattern *getSubPattern() const { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }

  TypeLoc &getTypeLoc() { return PatType; }
  TypeLoc getTypeLoc() const { return PatType; }

  SourceLoc getLoc() const { return SubPattern->getLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Typed;
  }
};

inline Pattern *Pattern::getSemanticsProvidingPattern() {
  if (ParenPattern *pp = dyn_cast<ParenPattern>(this))
    return pp->getSubPattern()->getSemanticsProvidingPattern();
  if (TypedPattern *tp = dyn_cast<TypedPattern>(this))
    return tp->getSubPattern()->getSemanticsProvidingPattern();
  return this;
}
  
/// A pattern which performs a dynamic type check. The match succeeds if the
/// class, archetype, or existential value is dynamically of the given type.
///
/// TODO: Introduce type refinement of the value being matched.
class IsaPattern : public Pattern {
  SourceLoc IsLoc;
  
  /// The semantics of the type check (class downcast, archetype-to-concrete,
  /// etc.)
  CheckedCastKind CastKind;
  
  /// The type being checked for.
  TypeLoc CastType;
  
public:
  IsaPattern(SourceLoc IsLoc, TypeLoc CastTy,
             CheckedCastKind Kind = CheckedCastKind::Unresolved)
    : Pattern(PatternKind::Isa),
      IsLoc(IsLoc),
      CastKind(Kind),
      CastType(CastTy) {
    assert(IsLoc.isValid() == CastTy.hasLocation());
    if (!IsLoc.isValid())
      setImplicit();
  }

  CheckedCastKind getCastKind() const { return CastKind; }
  void setCastKind(CheckedCastKind kind) { CastKind = kind; }
  
  SourceLoc getLoc() const { return IsLoc; }
  SourceRange getSourceRange() const {
    return {IsLoc, CastType.getSourceRange().End};
  }
  
  TypeLoc &getCastTypeLoc() { return CastType; }
  TypeLoc getCastTypeLoc() const { return CastType; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Isa;
  }
};

  
/// A pattern that matches a nominal type and destructures elements out of it.
/// The match succeeds if the matched value is dynamically of the specified
/// type and the subpattern matches the the value cast to the match type.
class NominalTypePattern : public Pattern {
  TypeLoc CastType;
  Pattern *SubPattern;
  CheckedCastKind Kind;
public:
  NominalTypePattern(TypeLoc CastTy, Pattern *Sub,
                     CheckedCastKind Kind = CheckedCastKind::Unresolved)
    : Pattern(PatternKind::NominalType), CastType(CastTy), SubPattern(Sub),
      Kind(Kind) {
    if (Sub->isImplicit())
      setImplicit();
  }

  const Pattern *getSubPattern() const { return SubPattern; }
  Pattern *getSubPattern() { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  TypeLoc &getCastTypeLoc() { return CastType; }
  TypeLoc getCastTypeLoc() const { return CastType; }
  
  CheckedCastKind getCastKind() const { return Kind; }
  void setCastKind(CheckedCastKind kind) { Kind = kind; }
  
  SourceLoc getLoc() const { return SubPattern->getLoc(); }
  SourceRange getSourceRange() const {
    return {CastType.getSourceRange().Start, SubPattern->getSourceRange().End};
  }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::NominalType;
  }
};
  
/// A pattern which matches a value obtained by evaluating an expression.
/// The match will be tested using user-defined '~=' operator function lookup;
/// the match succeeds if 'patternValue ~= matchedValue' produces a true value.
class ExprPattern : public Pattern {
  llvm::PointerIntPair<Expr *, 1, bool> SubExprAndIsResolved;
  
  /// An expression constructed during type-checking that produces a reference
  /// to the '~=' operator resolved for the match.
  Expr *MatchFnExpr;
  
public:
  ExprPattern(Expr *e, bool isResolved, Expr *match)
    : Pattern(PatternKind::Expr), SubExprAndIsResolved(e, isResolved),
      MatchFnExpr(match) {
    assert(!match || e->isImplicit() == match->isImplicit());
    if (e->isImplicit())
      setImplicit();
  }
  
  Expr *getSubExpr() const { return SubExprAndIsResolved.getPointer(); }
  void setSubExpr(Expr *e) { SubExprAndIsResolved.setPointer(e); }
  
  Expr *getMatchFnExpr() const { return MatchFnExpr; }
  void setMatchFnExpr(Expr *e) {
    assert(isResolved() && "cannot set match fn for unresolved expr patter");
    MatchFnExpr = e;
  }
  
  SourceLoc getLoc() const { return getSubExpr()->getLoc(); }
  SourceRange getSourceRange() const { return getSubExpr()->getSourceRange(); }
  
  /// True if pattern resolution has been applied to the subexpression.
  bool isResolved() const { return SubExprAndIsResolved.getInt(); }
  void setResolved(bool isResolved) { SubExprAndIsResolved.setInt(isResolved); }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Expr;
  }
};
  
/// A pattern which introduces variable bindings. This pattern node has no
/// semantics of its own, but has a syntactic effect on the subpattern. Bare
/// identifiers in the subpattern create new variable bindings instead of being
/// parsed as expressions referencing existing entities.
class VarPattern : public Pattern {
  SourceLoc VarLoc;
  Pattern *SubPattern;
public:
  VarPattern(SourceLoc loc, Pattern *sub)
    : Pattern(PatternKind::Var), VarLoc(loc), SubPattern(sub) {
    assert(loc.isValid() == !sub->isImplicit());
    if (sub->isImplicit())
      setImplicit();
  }
  
  SourceLoc getLoc() const { return VarLoc; }
  SourceRange getSourceRange() const {
    return {VarLoc, SubPattern->getSourceRange().End};
  }
  
  const Pattern *getSubPattern() const { return SubPattern; }
  Pattern *getSubPattern() { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Var;
  }
};
  
} // end namespace swift

#endif
