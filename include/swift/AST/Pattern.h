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
#include "swift/Basic/type_traits.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/OptionSet.h"

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
    unsigned HasVararg : 1;
    unsigned NumFields : NumBitsAllocated - NumPatternBits - 1;
  };

  class TypedPatternBitfields {
    friend class TypedPattern;
    unsigned : NumPatternBits;
    unsigned IsPropagatedType : 1;
  };

protected:
  union {
    PatternBitfields PatternBits;
    TuplePatternBitfields TuplePatternBits;
    TypedPatternBitfields TypedPatternBits;
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

  /// \brief Retrieve the name of the given pattern kind.
  ///
  /// This name should only be used for debugging dumps and other
  /// developer aids, and should never be part of a diagnostic or exposed
  /// to the user of the compiler in any way.
  static StringRef getKindName(PatternKind K);

  bool isImplicit() const { return PatternBits.isImplicit; }
  void setImplicit() { PatternBits.isImplicit = true; }

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

  /// If thie pattern has been type-checked, return the type it
  /// matches.
  Type getType() const { assert(hasType()); return Ty; }

  /// Set the type of this pattern, given that it was previously not
  /// type-checked.
  void setType(Type ty) { Ty = ty; }

  /// Overwrite the type of this pattern.
  void overwriteType(Type ty) { assert(hasType()); Ty = ty; }

  /// Returns the name directly bound by this pattern, or the null
  /// identifier if the pattern does not bind a name directly.
  Identifier getBoundName() const;

  /// Returns the name directly bound by this pattern within the body of
  /// a function, or the null identifier if the pattern does not bind a name directly.
  Identifier getBodyName() const;

  /// If this pattern binds a single variable without any
  /// destructuring or conditionalizing, return that variable.
  VarDecl *getSingleVar() const;

  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceLoc getLoc() const;

  /// \brief Collect the set of variables referenced in the given pattern.
  void collectVariables(SmallVectorImpl<VarDecl *> &variables) const;

  /// \brief apply the specified function to all variables referenced in this
  /// pattern.
  void forEachVariable(const std::function<void(VarDecl*)> &f) const;

  /// \brief apply the specified function to all pattern nodes recursively in
  /// this pattern.  This is a pre-order traversal.
  void forEachNode(const std::function<void(Pattern*)> &f);

  void forEachNode(const std::function<void(const Pattern*)> &f) const {
    const std::function<void(Pattern*)> &f2 = f;
    const_cast<Pattern *>(this)->forEachNode(f2);
  }

  /// \brief Mark all vardecls in this pattern as having non-pattern initial
  /// values bound into them.
  void markHasNonPatternBindingInit() {
    forEachVariable([&](VarDecl *VD) {
      VD->setHasNonPatternBindingInit();
    });
  }
  
  
  /// Return the number of "top-level" variables in the given pattern,
  /// which looks into one level of tuple pattern to determine the #
  /// of variables. If the pattern is not a tuple, the result is one.
  unsigned numTopLevelVariables() const;

  /// \brief Build an implicit 'self' parameter for the specified DeclContext.
  static Pattern *buildImplicitSelfParameter(SourceLoc Loc,
                                             TypeLoc TyLoc,
                                             DeclContext *CurDeclContext);

  /// \brief Build an implicit let parameter pattern for the specified
  /// DeclContext.
  static Pattern *buildImplicitLetParameter(SourceLoc loc, StringRef name,
                                            TypeLoc TyLoc,
                                            DeclContext *CurDeclContext);

  /// Flags used to indicate how pattern cloning should operate.
  enum CloneFlags {
    /// The cloned pattern should be implicit.
    Implicit = 0x01,
    /// The cloned pattern is for an inherited constructor; mark default
    /// arguments as inherited, and mark unnamed arguments as named.
    Inherited = 0x02,
    /// Whether the named patterns produced from a cloned 'any' pattern is
    /// are 'var'.
    IsVar = 0x04
  };

  Pattern *clone(ASTContext &context,
                 OptionSet<CloneFlags> options = None) const;

  /// Given that this is a function-parameter pattern, clone it in a
  /// way that permits makeForwardingReference to be called on the
  /// result.
  Pattern *cloneForwardable(ASTContext &context, DeclContext *DC,
                            OptionSet<CloneFlags> options = None) const;

  /// Form an un-typechecked reference to the variables bound by this
  /// pattern in a manner which perfectly forwards the values.  Not
  /// all patterns can be forwarded.
  Expr *buildForwardingRefExpr(ASTContext &context) const;
  
  static bool classof(const Pattern *P) { return true; }
  
  //*** Allocation Routines ************************************************/

  void *operator new(size_t bytes, ASTContext &C);

  // Make placement new and vanilla new/delete illegal for Patterns.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;
  void *operator new(size_t bytes, void *data) = delete;
  
  void print(llvm::raw_ostream &OS,
             const PrintOptions &Options = PrintOptions()) const;
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
class TuplePatternElt {
  Pattern *ThePattern;
  ExprHandle *Init;
  DefaultArgumentKind DefArgKind;

public:
  TuplePatternElt() = default;
  explicit TuplePatternElt(Pattern *P)
    : ThePattern(P), Init(nullptr), DefArgKind(DefaultArgumentKind::None) {}

  TuplePatternElt(Pattern *p, ExprHandle *init, DefaultArgumentKind defArgKind)
    : ThePattern(p), Init(init), DefArgKind(defArgKind) {}

  Pattern *getPattern() { return ThePattern; }
  const Pattern *getPattern() const { return ThePattern; }
  void setPattern(Pattern *p) { ThePattern = p; }

  ExprHandle *getInit() const { return Init; }

  DefaultArgumentKind getDefaultArgKind() const { return DefArgKind; }
  void setDefaultArgKind(DefaultArgumentKind DAK) { DefArgKind = DAK; }
};

/// A pattern consisting of a tuple of patterns.
class TuplePattern : public Pattern {
  SourceLoc LPLoc, RPLoc;
  // TuplePatternBits.HasVararg
  // TuplePatternBits.NumFields

  TuplePatternElt *getFieldsBuffer() {
    return reinterpret_cast<TuplePatternElt *>(this+1);
  }
  const TuplePatternElt *getFieldsBuffer() const {
    return reinterpret_cast<const TuplePatternElt *>(this + 1);
  }

  SourceLoc *getEllipsisLocPtr() {
    assert(TuplePatternBits.HasVararg);
    return reinterpret_cast<SourceLoc *>(getFieldsBuffer()+getNumFields());
  }
  const SourceLoc *getEllipsisLocPtr() const {
    assert(TuplePatternBits.HasVararg);
    return reinterpret_cast<const SourceLoc *>(getFieldsBuffer()+getNumFields());
  }

  TuplePattern(SourceLoc lp, unsigned numFields, SourceLoc rp, bool hasVararg,
               SourceLoc ellipsis, bool implicit)
      : Pattern(PatternKind::Tuple), LPLoc(lp), RPLoc(rp) {
    TuplePatternBits.NumFields = numFields;
    TuplePatternBits.HasVararg = hasVararg;
    if (hasVararg)
      *getEllipsisLocPtr() = ellipsis;
    assert(lp.isValid() == rp.isValid());
    if (implicit)
      setImplicit();
  }

public:
  static TuplePattern *create(ASTContext &C, SourceLoc lp,
                              ArrayRef<TuplePatternElt> elements, SourceLoc rp,
                              bool hasVararg = false,
                              SourceLoc ellipsis = SourceLoc(),
                              Optional<bool> implicit = None);

  /// \brief Create either a tuple pattern or a paren pattern, depending
  /// on the elements.
  static Pattern *createSimple(ASTContext &C, SourceLoc lp,
                               ArrayRef<TuplePatternElt> elements, SourceLoc rp,
                               bool hasVararg = false,
                               SourceLoc ellipsis = SourceLoc(),
                               Optional<bool> implicit = None);

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

  bool hasVararg() const { return TuplePatternBits.HasVararg; }

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const;
  SourceLoc getEllipsisLoc() const {
    if (hasVararg())
      return *getEllipsisLocPtr();
    return SourceLoc();
  }

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
  Identifier getBodyName() const;
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
  TypeLoc PatType;

public:
  TypedPattern(Pattern *pattern, TypeLoc tl, Optional<bool> implicit = None)
    : Pattern(PatternKind::Typed), SubPattern(pattern), PatType(tl) {
    if (implicit.hasValue() ? *implicit : !tl.hasLocation())
      setImplicit();
    TypedPatternBits.IsPropagatedType = false;
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
    return TypedPatternBits.IsPropagatedType;
  }
  void setPropagatedType() {
    TypedPatternBits.IsPropagatedType = true;
  }

  Pattern *getSubPattern() { return SubPattern; }
  const Pattern *getSubPattern() const { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }

  TypeLoc &getTypeLoc() { return PatType; }
  TypeLoc getTypeLoc() const { return PatType; }

  SourceLoc getLoc() const {
    if (SubPattern->isImplicit())
      return PatType.getSourceRange().Start;

    return SubPattern->getLoc();
  }
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
             CheckedCastKind Kind = CheckedCastKind::Unresolved,
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
  
  Pattern *getSubPattern() { return SubPattern; }
  const Pattern *getSubPattern() const { return SubPattern; }
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  SourceLoc getLoc() const { return IsLoc; }
  SourceRange getSourceRange() const {
    return {SubPattern ? SubPattern->getSourceRange().Start : IsLoc,
            CastType.getSourceRange().End};
  }
  
  TypeLoc &getCastTypeLoc() { return CastType; }
  TypeLoc getCastTypeLoc() const { return CastType; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Is;
  }
};

  
/// A pattern that matches a nominal type and destructures elements out of it.
/// The match succeeds if the loaded property values all match their associated
/// subpatterns.
class NominalTypePattern : public Pattern {
public:
  /// A nominal type subpattern record.
  class Element {
    /// The location of the property name.
    SourceLoc PropertyLoc;
    /// The location of the colon.
    SourceLoc ColonLoc;
    /// The referenced property name.
    Identifier PropertyName;
    /// The referenced property.
    VarDecl *Property;
    /// The subpattern.
    Pattern *SubPattern;
  public:
    Element(SourceLoc PropLoc, Identifier PropName, VarDecl *Prop,
            SourceLoc ColonLoc,
            Pattern *SubP)
      : PropertyLoc(PropLoc), ColonLoc(ColonLoc),
        PropertyName(PropName), Property(Prop),
        SubPattern(SubP)
    {}
    
    SourceLoc getPropertyLoc() const { return PropertyLoc; }
    SourceLoc getColonLoc() const { return ColonLoc; }
    
    VarDecl *getProperty() const { return Property; }
    void setProperty(VarDecl *v) { Property = v; }
    
    Identifier getPropertyName() const { return PropertyName; }
    
    const Pattern *getSubPattern() const { return SubPattern; }
    Pattern *getSubPattern() { return SubPattern; }
    void setSubPattern(Pattern *p) { SubPattern = p; }
  };
  
private:
  TypeLoc CastType;
  SourceLoc LParenLoc, RParenLoc;
  
  unsigned NumElements;
  
  Element *getElementStorage() {
    return reinterpret_cast<Element *>(this + 1);
  }
  const Element *getElementStorage() const {
    return reinterpret_cast<const Element *>(this + 1);
  }
  
  NominalTypePattern(TypeLoc CastTy, SourceLoc LParenLoc,
                     ArrayRef<Element> Elements,
                     SourceLoc RParenLoc,
                     Optional<bool> implicit = None)
    : Pattern(PatternKind::NominalType), CastType(CastTy),
      LParenLoc(LParenLoc), RParenLoc(RParenLoc),
      NumElements(Elements.size())
  {
    if (implicit.hasValue() ? *implicit : !CastTy.hasLocation())
      setImplicit();
    static_assert(IsTriviallyCopyable<Element>::value,
                  "assuming Element is trivially copyable");
    memcpy(getElementStorage(), Elements.begin(),
           Elements.size() * sizeof(Element));
  }
  
public:
  static NominalTypePattern *create(TypeLoc CastTy, SourceLoc LParenLoc,
                                    ArrayRef<Element> Elements,
                                    SourceLoc RParenLoc,
                                    ASTContext &C,
                                    Optional<bool> implicit = None);

  TypeLoc &getCastTypeLoc() { return CastType; }
  TypeLoc getCastTypeLoc() const { return CastType; }
  
  ArrayRef<Element> getElements() const {
    return {getElementStorage(), NumElements};
  }
  MutableArrayRef<Element> getMutableElements() {
    return {getElementStorage(), NumElements};
  }
  
  SourceLoc getLoc() const { return CastType.getSourceRange().Start; }
  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }
  SourceRange getSourceRange() const {
    return {getLoc(), RParenLoc};
  }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::NominalType;
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
  EnumElementDecl *ElementDecl;
  Pattern /*nullable*/ *SubPattern;
  
public:
  EnumElementPattern(TypeLoc ParentType, SourceLoc DotLoc, SourceLoc NameLoc,
                     Identifier Name, EnumElementDecl *Element,
                     Pattern *SubPattern, Optional<bool> Implicit = None)
    : Pattern(PatternKind::EnumElement),
      ParentType(ParentType), DotLoc(DotLoc), NameLoc(NameLoc), Name(Name),
      ElementDecl(Element), SubPattern(SubPattern) {
    if (Implicit.hasValue() ? *Implicit : !ParentType.hasLocation())
      setImplicit();
  }

  bool hasSubPattern() const { return SubPattern; }
  
  const Pattern *getSubPattern() const {
    return SubPattern;
  }
  
  Pattern *getSubPattern() {
    return SubPattern;
  }
  
  void setSubPattern(Pattern *p) { SubPattern = p; }
  
  Identifier getName() const { return Name; }
  
  EnumElementDecl *getElementDecl() const { return ElementDecl; }
  void setElementDecl(EnumElementDecl *d) { ElementDecl = d; }
  
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }
  SourceLoc getStartLoc() const {
    return ParentType.hasLocation() ? ParentType.getSourceRange().Start :
           DotLoc.isValid()         ? DotLoc
                                    : NameLoc;
  }
  SourceLoc getEndLoc() const {
    return SubPattern ? SubPattern->getSourceRange().End : NameLoc;
  }
  SourceRange getSourceRange() const { return {getStartLoc(), getEndLoc()}; }
  
  TypeLoc getParentType() const { return ParentType; }
  
  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::EnumElement;
  }
};

/// A pattern that matches an enum case. If the enum value is in the matching
/// case, then the value is extracted. If there is a subpattern, it is then
/// matched against the associated value for the case.
class BoolPattern : public Pattern {
  TypeLoc ParentType;
  SourceLoc DotLoc;
  SourceLoc NameLoc;
  Identifier Name;
  Expr *BoolValue;
  Pattern /*nullable*/ *SubPattern;

public:
  BoolPattern(TypeLoc ParentType, SourceLoc DotLoc, SourceLoc NameLoc,
                     Identifier Name, Expr *BoolValue,
                     Pattern *SubPattern, Optional<bool> Implicit = None)
    : Pattern(PatternKind::Bool),
      ParentType(ParentType), DotLoc(DotLoc), NameLoc(NameLoc), Name(Name),
      BoolValue(BoolValue), SubPattern(SubPattern) {
    if (Implicit.hasValue() ? *Implicit : !ParentType.hasLocation())
      setImplicit();
  }

  bool hasSubPattern() const { return SubPattern; }

  const Pattern *getSubPattern() const {
    return SubPattern;
  }

  Pattern *getSubPattern() {
    return SubPattern;
  }

  void setSubPattern(Pattern *p) { SubPattern = p; }

  Identifier getName() const { return Name; }

  Expr *getBoolValue() const { return BoolValue; }
  void setBoolValue(Expr *v) { BoolValue = v; }

  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }
  SourceLoc getStartLoc() const {
    return ParentType.hasLocation() ? ParentType.getSourceRange().Start :
           DotLoc.isValid()         ? DotLoc
                                    : NameLoc;
  }
  SourceLoc getEndLoc() const {
    return SubPattern ? SubPattern->getSourceRange().End : NameLoc;
  }
  SourceRange getSourceRange() const { return {getStartLoc(), getEndLoc()}; }

  TypeLoc getParentType() const { return ParentType; }

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
              Optional<bool> implicit = None)
    : Pattern(PatternKind::Expr), SubExprAndIsResolved(e, isResolved),
      MatchExpr(matchExpr), MatchVar(matchVar) {
    assert(!matchExpr || e->isImplicit() == matchExpr->isImplicit());
    if (implicit.hasValue() ? *implicit : e->isImplicit())
      setImplicit();
  }
  
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
  bool IsLet;  // True if this is a let pattern, false if a var pattern.
  Pattern *SubPattern;
public:
  VarPattern(SourceLoc loc, bool isLet, Pattern *sub,
             Optional<bool> implicit = None)
    : Pattern(PatternKind::Var), VarLoc(loc), IsLet(isLet), SubPattern(sub) {
    if (implicit.hasValue() ? *implicit : !loc.isValid())
      setImplicit();
  }

  bool isLet() const { return IsLet; }
  
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
