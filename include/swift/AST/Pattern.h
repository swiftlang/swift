//===--- Pattern.h - Swift Language Pattern-Matching ASTs -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Pattern class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PATTERN_H
#define SWIFT_PATTERN_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/type_traits.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/InlineBitfield.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
  class ASTContext;
  class Expr;
  enum class CheckedCastKind : unsigned;

/// PatternKind - The classification of different kinds of
/// value-matching pattern.
enum class PatternKind : uint8_t {
#define PATTERN(ID, PARENT) ID,
#define LAST_PATTERN(ID) Last_Pattern = ID,
#include "PatternNodes.def"
};
enum : unsigned { NumPatternKindBits =
  countBitsUsed(static_cast<unsigned>(PatternKind::Last_Pattern)) };

/// Diagnostic printing of PatternKinds.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, PatternKind kind);
  
/// Pattern - Base class for all patterns in Swift.
class alignas(8) Pattern {
protected:
  union { uint64_t OpaqueBits;

  SWIFT_INLINE_BITFIELD_BASE(Pattern, bitmax(NumPatternKindBits,8)+1+1,
    Kind : bitmax(NumPatternKindBits,8),
    isImplicit : 1,
    hasInterfaceType : 1
  );

  SWIFT_INLINE_BITFIELD_FULL(TuplePattern, Pattern, 32,
    : NumPadBits,
    NumElements : 32
  );

  SWIFT_INLINE_BITFIELD(TypedPattern, Pattern, 1,
    IsPropagatedType : 1
  );

  SWIFT_INLINE_BITFIELD(BoolPattern, Pattern, 1,
    Value : 1
  );

  SWIFT_INLINE_BITFIELD(VarPattern, Pattern, 1,
    /// True if this is a let pattern, false if a var pattern.
    IsLet : 1
  );

  } Bits;

  Pattern(PatternKind kind) {
    Bits.OpaqueBits = 0;
    Bits.Pattern.Kind = unsigned(kind);
    Bits.Pattern.isImplicit = false;
    Bits.Pattern.hasInterfaceType = false;
  }

private:
  /// The checked type of the pattern.
  ///
  /// if \c Bits.Pattern.hasInterfaceType, this stores the interface type of the
  /// pattern, which will be lazily resolved to the contextual type using
  /// the environment in \c ASTContext::DelayedPatternContexts.
  mutable Type Ty;

public:
  PatternKind getKind() const { return PatternKind(Bits.Pattern.Kind); }

  /// Retrieve the name of the given pattern kind.
  ///
  /// This name should only be used for debugging dumps and other
  /// developer aids, and should never be part of a diagnostic or exposed
  /// to the user of the compiler in any way.
  static StringRef getKindName(PatternKind K);

  /// A pattern is implicit if it is compiler-generated and there
  /// exists no source code for it.
  bool isImplicit() const { return Bits.Pattern.isImplicit; }
  void setImplicit() { Bits.Pattern.isImplicit = true; }

  /// Find the smallest subpattern which obeys the property that matching it is
  /// equivalent to matching this pattern.
  ///
  /// Looks through ParenPattern, VarPattern, and TypedPattern.
  Pattern *getSemanticsProvidingPattern();
  const Pattern *getSemanticsProvidingPattern() const {
    return const_cast<Pattern*>(this)->getSemanticsProvidingPattern();
  }

  /// Returns whether this pattern has been type-checked yet.
  bool hasType() const { return !Ty.isNull(); }

  /// If this pattern has been type-checked, return the type it
  /// matches.
  Type getType() const;

  /// Set the type of this pattern, given that it was previously not
  /// type-checked.
  void setType(Type ty) {
    assert(!ty || !ty->hasTypeVariable());
    Ty = ty;
  }

  /// Retrieve the delayed interface type of this pattern, if it has one.
  ///
  /// Note: this is used for delayed deserialization logic.
  Type getDelayedInterfaceType() const {
    if (Bits.Pattern.hasInterfaceType) return Ty;
    return nullptr;
  }

  /// Set the type of this pattern as an interface type whose resolution to
  /// a context type will be performed lazily.
  ///
  /// \param dc The context in which the type will be resolved.
  void setDelayedInterfaceType(Type interfaceTy, DeclContext *dc);

  /// Overwrite the type of this pattern.
  void overwriteType(Type ty) { assert(hasType()); Ty = ty; }

  /// Returns the name directly bound by this pattern, or the null
  /// identifier if the pattern does not bind a name directly.
  Identifier getBoundName() const;

  /// If this pattern binds a single variable without any
  /// destructuring or conditionalizing, return that variable.
  VarDecl *getSingleVar() const;

  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceLoc getLoc() const;

  /// Collect the set of variables referenced in the given pattern.
  void collectVariables(SmallVectorImpl<VarDecl *> &variables) const;

  /// apply the specified function to all variables referenced in this
  /// pattern.
  void forEachVariable(llvm::function_ref<void(VarDecl *)> f) const;

  /// Returns true if \p vd is in the pattern.
  bool containsVarDecl(const VarDecl *inputVD) const {
    bool result = false;
    forEachVariable([&](VarDecl *vd) { result |= inputVD == vd; });
    return result;
  }

  /// apply the specified function to all pattern nodes recursively in
  /// this pattern.  This is a pre-order traversal.
  void forEachNode(llvm::function_ref<void(Pattern *)> f);

  void forEachNode(llvm::function_ref<void(const Pattern *)> f) const {
    llvm::function_ref<void(Pattern *)> f2 = f;
    const_cast<Pattern *>(this)->forEachNode(f2);
  }

  /// Return true if this pattern (or a subpattern) is refutable.
  bool isRefutablePattern() const;

  bool isNeverDefaultInitializable() const;

  /// Mark all vardecls in this pattern as having non-pattern initial
  /// values bound into them.
  void markHasNonPatternBindingInit() {
    forEachVariable([&](VarDecl *VD) {
      VD->setHasNonPatternBindingInit();
    });
  }
  
  /// Mark all vardecls in this pattern as having an owning statement for
  /// the pattern.
  void markOwnedByStatement(Stmt *S) {
    forEachVariable([&](VarDecl *VD) {
      VD->setParentPatternStmt(S);
    });
  }

  /// Does this binding declare something that requires storage?
  bool hasStorage() const;

  static bool classof(const Pattern *P) { return true; }
  
  //*** Allocation Routines ************************************************/

  void *operator new(size_t bytes, const ASTContext &C);

  // Make placement new and vanilla new/delete illegal for Patterns.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;
  void *operator new(size_t bytes, void *data) = delete;
  
  void print(llvm::raw_ostream &OS,
             const PrintOptions &Options = PrintOptions()) const;
  SWIFT_DEBUG_DUMP;
  
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
  ParenPattern(SourceLoc lp, Pattern *sub, SourceLoc rp,
               Optional<bool> implicit = None)
    : Pattern(PatternKind::Paren),
      LPLoc(lp), RPLoc(rp), SubPattern(sub) {
    assert(lp.isValid() == rp.isValid());
    if (implicit.hasValue() ? *implicit : !lp.isValid())
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
///
/// The fully general form of this is something like:
///    label: (pattern) = initexpr
///
/// The Init and DefArgKind fields are only used in argument lists for
/// functions.  They are not parsed as part of normal pattern grammar.
class TuplePatternElt {
  Identifier Label;
  SourceLoc LabelLoc;
  Pattern *ThePattern;

public:
  TuplePatternElt() = default;
  explicit TuplePatternElt(Pattern *P) : ThePattern(P) {}

  TuplePatternElt(Identifier Label, SourceLoc LabelLoc, Pattern *p)
    : Label(Label), LabelLoc(LabelLoc), ThePattern(p) {}

  Identifier getLabel() const { return Label; }
  SourceLoc getLabelLoc() const { return LabelLoc; }
  void setLabel(Identifier I, SourceLoc Loc) {
    Label = I;
    LabelLoc = Loc;
  }

  Pattern *getPattern() { return ThePattern; }
  const Pattern *getPattern() const {
    return ThePattern;
  }
  
  void setPattern(Pattern *p) { ThePattern = p; }
};

/// A pattern consisting of a tuple of patterns.
class TuplePattern final : public Pattern,
    private llvm::TrailingObjects<TuplePattern, TuplePatternElt> {
  friend TrailingObjects;
  SourceLoc LPLoc, RPLoc;
  // Bits.TuplePattern.NumElements

  TuplePattern(SourceLoc lp, unsigned numElements, SourceLoc rp,
               bool implicit)
      : Pattern(PatternKind::Tuple), LPLoc(lp), RPLoc(rp) {
    Bits.TuplePattern.NumElements = numElements;
    assert(lp.isValid() == rp.isValid());
    if (implicit)
      setImplicit();
  }

public:
  static TuplePattern *create(ASTContext &C, SourceLoc lp,
                              ArrayRef<TuplePatternElt> elements, SourceLoc rp,
                              Optional<bool> implicit = None);

  /// Create either a tuple pattern or a paren pattern, depending
  /// on the elements.
  static Pattern *createSimple(ASTContext &C, SourceLoc lp,
                               ArrayRef<TuplePatternElt> elements, SourceLoc rp,
                               Optional<bool> implicit = None);

  unsigned getNumElements() const {
    return Bits.TuplePattern.NumElements;
  }

  MutableArrayRef<TuplePatternElt> getElements() {
    return {getTrailingObjects<TuplePatternElt>(), getNumElements()};
  }
  ArrayRef<TuplePatternElt> getElements() const {
    return {getTrailingObjects<TuplePatternElt>(), getNumElements()};
  }

  const TuplePatternElt &getElement(unsigned i) const {return getElements()[i];}
  TuplePatternElt &getElement(unsigned i) { return getElements()[i]; }

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const;

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Tuple;
  }
};

/// A pattern which binds a name to an arbitrary value of its type.
class NamedPattern : public Pattern {
  VarDecl *const Var;

public:
  explicit NamedPattern(VarDecl *Var, Optional<bool> implicit = None)
      : Pattern(PatternKind::Named), Var(Var) {
    if (implicit.hasValue() ? *implicit : !Var->getLoc().isValid())
      setImplicit();
  }

  VarDecl *getDecl() const { return Var; }
  Identifier getBoundName() const;
  StringRef getNameStr() const { return Var->getNameStr(); }

  SourceLoc getLoc() const { return Var->getLoc(); }
  SourceRange getSourceRange() const { return Var->getSourceRange(); }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Named;
  }
};

/// A pattern which matches an arbitrary value of a type, but does not
/// bind a name to it.  This is spelled "_".
class AnyPattern : public Pattern {
  SourceLoc Loc;

public:
  explicit AnyPattern(SourceLoc Loc, Optional<bool> implicit = None)
      : Pattern(PatternKind::Any), Loc(Loc) {
    if (implicit.hasValue() ? *implicit : !Loc.isValid())
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
/// a value of the type. This is different from IsPattern, which is a refutable
/// dynamic type match.
class TypedPattern : public Pattern {
  Pattern *SubPattern;
  TypeRepr *PatTypeRepr;

public:
  /// Creates a new TypedPattern which annotates the provided sub-pattern with
  /// the provided TypeRepr. If 'implicit' is true, the pattern will be
  /// set to implicit. If false, it will not. If 'implicit' is not provided,
  /// then the pattern will be set to 'implicit' if there is a provided TypeRepr
  /// which has a valid SourceRange.
  TypedPattern(Pattern *pattern, TypeRepr *tr, Optional<bool> implicit = None);

  /// Creates an implicit typed pattern annotating the provided sub-pattern
  /// with a given type.
  static TypedPattern *
  createImplicit(ASTContext &ctx, Pattern *pattern, Type type) {
    auto tp = new (ctx) TypedPattern(pattern, /*typeRepr*/nullptr,
                                     /*implicit*/true);
    if (!type.isNull())
      tp->setType(type);
    return tp;
  }

  /// True if the type in this \c TypedPattern was propagated from a different
  /// \c TypedPattern.
  ///
  /// For example, in:
  /// \code
  ///   var a, b: Int, c, d: Double
  /// \endcode
  /// 'a' and 'c' will have this bit set to true.
  bool isPropagatedType() const {
    return Bits.TypedPattern.IsPropagatedType;
  }
  void setPropagatedType() {
    Bits.TypedPattern.IsPropagatedType = true;
  }

  Pattern *getSubPattern() { return SubPattern; }
  const Pattern *getSubPattern() const { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }

  TypeRepr *getTypeRepr() const { return PatTypeRepr; }

  TypeLoc getTypeLoc() const;
  SourceLoc getLoc() const;
  SourceRange getSourceRange() const;

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Typed;
  }
};

/// A pattern which performs a dynamic type check. The match succeeds if the
/// class, archetype, or existential value is dynamically of the given type.
///
/// TODO: Introduce type refinement of the value being matched.
class IsPattern : public Pattern {
  SourceLoc IsLoc;
  
  Pattern *SubPattern;
  
  /// The semantics of the type check (class downcast, archetype-to-concrete,
  /// etc.)
  CheckedCastKind CastKind;
  
  /// The type being checked for.
  TypeLoc CastType;
  
public:
  IsPattern(SourceLoc IsLoc, TypeLoc CastTy,
             Pattern *SubPattern,
             CheckedCastKind Kind,
             Optional<bool> implicit = None)
    : Pattern(PatternKind::Is),
      IsLoc(IsLoc),
      SubPattern(SubPattern),
      CastKind(Kind),
      CastType(CastTy) {
    assert(IsLoc.isValid() == CastTy.hasLocation());
    if (implicit.hasValue() ? *implicit : !IsLoc.isValid())
      setImplicit();
  }

  CheckedCastKind getCastKind() const { return CastKind; }
  void setCastKind(CheckedCastKind kind) { CastKind = kind; }

  bool hasSubPattern() const { return SubPattern; }
  Pattern *getSubPattern() { return SubPattern; }
  const Pattern *getSubPattern() const { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  SourceLoc getLoc() const { return IsLoc; }
  SourceRange getSourceRange() const {
    SourceLoc beginLoc =
      SubPattern ? SubPattern->getSourceRange().Start : IsLoc;
    SourceLoc endLoc =
      (isImplicit() ? beginLoc : CastType.getSourceRange().End);
    return { beginLoc, endLoc };
  }
  
  TypeLoc &getCastTypeLoc() { return CastType; }
  TypeLoc getCastTypeLoc() const { return CastType; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Is;
  }
};
  
/// A pattern that matches an enum case. If the enum value is in the matching
/// case, then the value is extracted. If there is a subpattern, it is then
/// matched against the associated value for the case.
class EnumElementPattern : public Pattern {
  TypeLoc ParentType;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  Identifier Name;
  PointerUnion<EnumElementDecl *, Expr*> ElementDeclOrUnresolvedOriginalExpr;
  Pattern /*nullable*/ *SubPattern;
  
public:
  EnumElementPattern(TypeLoc ParentType, SourceLoc DotLoc, SourceLoc NameLoc,
                     Identifier Name, EnumElementDecl *Element,
                     Pattern *SubPattern, Optional<bool> Implicit = None)
    : Pattern(PatternKind::EnumElement),
      ParentType(ParentType), DotLoc(DotLoc), NameLoc(NameLoc), Name(Name),
      ElementDeclOrUnresolvedOriginalExpr(Element),
      SubPattern(SubPattern) {
    if (Implicit.hasValue() && *Implicit)
      setImplicit();
  }
  
  /// Create an unresolved EnumElementPattern for a `.foo` pattern relying on
  /// contextual type.
  EnumElementPattern(SourceLoc DotLoc,
                     SourceLoc NameLoc,
                     Identifier Name,
                     Pattern *SubPattern,
                     Expr *UnresolvedOriginalExpr)
    : Pattern(PatternKind::EnumElement),
      ParentType(), DotLoc(DotLoc), NameLoc(NameLoc), Name(Name),
      ElementDeclOrUnresolvedOriginalExpr(UnresolvedOriginalExpr),
      SubPattern(SubPattern) {
    
  }

  bool hasSubPattern() const { return SubPattern; }
  
  const Pattern *getSubPattern() const {
    return SubPattern;
  }
  
  Pattern *getSubPattern() {
    return SubPattern;
  }

  bool isParentTypeImplicit() {
    return !ParentType.hasLocation();
  }
  
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  Identifier getName() const { return Name; }
  
  EnumElementDecl *getElementDecl() const {
    return ElementDeclOrUnresolvedOriginalExpr.dyn_cast<EnumElementDecl*>();
  }
  void setElementDecl(EnumElementDecl *d) {
    ElementDeclOrUnresolvedOriginalExpr = d;
  }
  
  Expr *getUnresolvedOriginalExpr() const {
    return ElementDeclOrUnresolvedOriginalExpr.get<Expr*>();
  }
  bool hasUnresolvedOriginalExpr() const {
    return ElementDeclOrUnresolvedOriginalExpr.is<Expr*>();
  }
  
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }
  SourceLoc getStartLoc() const {
    return ParentType.hasLocation() ? ParentType.getSourceRange().Start :
           DotLoc.isValid()         ? DotLoc
                                    : NameLoc;
  }
  SourceLoc getEndLoc() const {
    if (SubPattern && SubPattern->getSourceRange().isValid()) {
      return SubPattern->getSourceRange().End;
    }
    return NameLoc;
  }
  SourceRange getSourceRange() const { return {getStartLoc(), getEndLoc()}; }
  
  TypeLoc &getParentType() { return ParentType; }
  TypeLoc getParentType() const { return ParentType; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::EnumElement;
  }
};

/// A pattern that matches an enum case. If the enum value is in the matching
/// case, then the value is extracted. If there is a subpattern, it is then
/// matched against the associated value for the case.
class BoolPattern : public Pattern {
  SourceLoc NameLoc;

public:
  BoolPattern(SourceLoc NameLoc, bool Value)
      : Pattern(PatternKind::Bool), NameLoc(NameLoc) {
    Bits.BoolPattern.Value = Value;
  }

  bool getValue() const { return Bits.BoolPattern.Value; }
  void setValue(bool v) { Bits.BoolPattern.Value = v; }

  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }
  SourceLoc getStartLoc() const {
    return NameLoc;
  }
  SourceLoc getEndLoc() const {
    return NameLoc;
  }
  SourceRange getSourceRange() const { return {getStartLoc(), getEndLoc()}; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Bool;
  }
};

/// A pattern "x?" which matches ".Some(x)".
class OptionalSomePattern : public Pattern {
  Pattern *SubPattern;
  SourceLoc QuestionLoc;
  EnumElementDecl *ElementDecl = nullptr;

public:
  explicit OptionalSomePattern(Pattern *SubPattern,
                               SourceLoc QuestionLoc,
                               Optional<bool> implicit = None)
  : Pattern(PatternKind::OptionalSome), SubPattern(SubPattern),
    QuestionLoc(QuestionLoc) {
    if (implicit.hasValue() ? *implicit : !QuestionLoc.isValid())
      setImplicit();
  }

  SourceLoc getQuestionLoc() const { return QuestionLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(SubPattern->getStartLoc(), QuestionLoc);
  }

  const Pattern *getSubPattern() const { return SubPattern; }
  Pattern *getSubPattern() { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }

  EnumElementDecl *getElementDecl() const { return ElementDecl; }
  void setElementDecl(EnumElementDecl *d) { ElementDecl = d; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::OptionalSome;
  }
};


  
/// A pattern which matches a value obtained by evaluating an expression.
/// The match will be tested using user-defined '~=' operator function lookup;
/// the match succeeds if 'patternValue ~= matchedValue' produces a true value.
class ExprPattern : public Pattern {
  llvm::PointerIntPair<Expr *, 1, bool> SubExprAndIsResolved;
  
  /// An expression constructed during type-checking that produces a call to the
  /// '~=' operator comparing the match expression on the left to the matched
  /// value on the right.
  Expr *MatchExpr;
  
  /// An implicit variable used to represent the RHS value of the match.
  VarDecl *MatchVar;
  
public:
  /// Construct an ExprPattern.
  ExprPattern(Expr *e, bool isResolved, Expr *matchExpr, VarDecl *matchVar,
              Optional<bool> implicit = None);
  
  /// Construct an unresolved ExprPattern.
  ExprPattern(Expr *e)
    : ExprPattern(e, false, nullptr, nullptr)
  {}
  
  /// Construct a resolved ExprPattern.
  ExprPattern(Expr *e, Expr *matchExpr, VarDecl *matchVar)
    : ExprPattern(e, true, matchExpr, matchVar)
  {}
  
  Expr *getSubExpr() const { return SubExprAndIsResolved.getPointer(); }
  void setSubExpr(Expr *e) { SubExprAndIsResolved.setPointer(e); }
  
  Expr *getMatchExpr() const { return MatchExpr; }
  void setMatchExpr(Expr *e) {
    assert(isResolved() && "cannot set match fn for unresolved expr patter");
    MatchExpr = e;
  }
  
  VarDecl *getMatchVar() const { return MatchVar; }
  void setMatchVar(VarDecl *v) {
    assert(isResolved() && "cannot set match var for unresolved expr patter");
    MatchVar = v;
  }
  
  SourceLoc getLoc() const;
  SourceRange getSourceRange() const;
  
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
  VarPattern(SourceLoc loc, bool isLet, Pattern *sub,
             Optional<bool> implicit = None)
      : Pattern(PatternKind::Var), VarLoc(loc), SubPattern(sub) {
    Bits.VarPattern.IsLet = isLet;
    if (implicit.hasValue() ? *implicit : !loc.isValid())
      setImplicit();
  }

  bool isLet() const { return Bits.VarPattern.IsLet; }
  
  SourceLoc getLoc() const { return VarLoc; }
  SourceRange getSourceRange() const {
    SourceLoc EndLoc = SubPattern->getSourceRange().End;
    if (EndLoc.isInvalid())
      return VarLoc;
    return {VarLoc, EndLoc};
  }
  
  const Pattern *getSubPattern() const { return SubPattern; }
  Pattern *getSubPattern() { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Var;
  }
};

  
inline Pattern *Pattern::getSemanticsProvidingPattern() {
  if (auto *pp = dyn_cast<ParenPattern>(this))
    return pp->getSubPattern()->getSemanticsProvidingPattern();
  if (auto *tp = dyn_cast<TypedPattern>(this))
    return tp->getSubPattern()->getSemanticsProvidingPattern();
  if (auto *vp = dyn_cast<VarPattern>(this))
    return vp->getSubPattern()->getSemanticsProvidingPattern();
  return this;
}
  
} // end namespace swift

#endif
