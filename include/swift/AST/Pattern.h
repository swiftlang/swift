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
#include "swift/AST/LLVM.h"
#include "swift/AST/Type.h"

namespace swift {
  class ASTContext;
  class ExprHandle;
  class TypeLoc;

/// PatternKind - The classification of different kinds of
/// value-matching pattern.
enum class PatternKind {
#define PATTERN(ID, PARENT) ID,
#include "PatternNodes.def"
};

/// Pattern - Base class for all patterns in Swift.
class Pattern {
  class PatternBitfields {
    friend class Pattern;
    unsigned Kind : 8;
  };
  enum { NumPatternBits = 8 };
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

  Pattern(PatternKind kind, Type type = Type()) : Ty(type) {
    PatternBits.Kind = unsigned(kind);
  }

private:
  /// The checked type of the pattern.
  Type Ty;

public:
  PatternKind getKind() const { return PatternKind(PatternBits.Kind); }

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
  void setType(Type ty) { assert(!hasType()); Ty = ty; }

  /// Overwrite the type of this pattern.
  void overwriteType(Type ty) { assert(hasType()); Ty = ty; }

  /// Returns the name directly bound by this pattern, or the null
  /// identifier if the pattern does not bind a name directly.
  Identifier getBoundName() const;

  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceLoc getLoc() const;

  static bool classof(const Pattern *P) { return true; }

  //*** Allocation Routines ************************************************/

  enum { Alignment = 8U };

  void *operator new(size_t bytes, ASTContext &C);

  // Make placement new and vanilla new/delete illegal for Patterns.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;
  void *operator new(size_t bytes, void *data) = delete;
};

/// A pattern consisting solely of grouping parentheses around a
/// different pattern.
class ParenPattern : public Pattern {
  SourceLoc LPLoc, RPLoc;
  Pattern *SubPattern;
public:
  ParenPattern(SourceLoc lp, Pattern *sub, SourceLoc rp, Type type = Type())
    : Pattern(PatternKind::Paren, type),
      LPLoc(lp), RPLoc(rp), SubPattern(sub) {}

  Pattern *getSubPattern() const { return SubPattern; }

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const { return SourceRange(LPLoc, RPLoc); }
  SourceLoc getLoc() const { return SubPattern->getLoc(); }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Paren;
  }
  static bool classof(const ParenPattern *P) { return true; }
};

/// An element of a tuple pattern.
class TuplePatternElt {
  Pattern *ThePattern;
  ExprHandle *Init;
  Type VarargBaseType;

public:
  TuplePatternElt() = default;
  explicit TuplePatternElt(Pattern *P, ExprHandle *init = nullptr,
                           Type varargBaseType = Type())
    : ThePattern(P), Init(init), VarargBaseType(varargBaseType) {}

  Pattern *getPattern() const { return ThePattern; }
  ExprHandle *getInit() const { return Init; }
  bool isVararg() { return !VarargBaseType.isNull(); }
  Type getVarargBaseType() { return VarargBaseType; }
  void setVarargBaseType(Type ty) { VarargBaseType = ty; }
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
  }

public:
  static TuplePattern *create(ASTContext &C, SourceLoc lp,
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
  static bool classof(const TuplePattern *P) { return true; }
};

/// A pattern which binds a name to an arbitrary value of its type.
class NamedPattern : public Pattern {
  VarDecl *const Var;

public:
  NamedPattern(VarDecl *var) : Pattern(PatternKind::Named), Var(var) {}

  VarDecl *getDecl() const { return Var; }
  Identifier getBoundName() const { return Var->getName(); }

  SourceLoc getLoc() const { return Var->getLoc(); }
  SourceRange getSourceRange() const { return getLoc(); }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Named;
  }
  static bool classof(const NamedPattern *P) { return true; }
};

/// A pattern which matches an arbitrary value of a type, but does not
/// bind a name to it.
class AnyPattern : public Pattern {
  SourceLoc Loc;

public:
  AnyPattern(SourceLoc loc) : Pattern(PatternKind::Any), Loc(loc) {}

  SourceLoc getLoc() const { return Loc; }
  SourceRange getSourceRange() const { return Loc; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Any;
  }
  static bool classof(const AnyPattern *P) { return true; }
};

/// A pattern which matches a sub-pattern and annotates it with a
/// type.
class TypedPattern : public Pattern {
  Pattern *SubPattern;
  TypeLoc *PatTypeLoc;

public:
  TypedPattern(Pattern *pattern, Type type, TypeLoc *tl)
    : Pattern(PatternKind::Typed, type), SubPattern(pattern), PatTypeLoc(tl) {}

  Pattern *getSubPattern() const { return SubPattern; }
  TypeLoc *getTypeLoc() const { return PatTypeLoc; }

  SourceLoc getLoc() const { return SubPattern->getLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Typed;
  }
  static bool classof(const TypedPattern *P) { return true; }
};

inline Pattern *Pattern::getSemanticsProvidingPattern() {
  if (ParenPattern *pp = dyn_cast<ParenPattern>(this))
    return pp->getSubPattern()->getSemanticsProvidingPattern();
  if (TypedPattern *tp = dyn_cast<TypedPattern>(this))
    return tp->getSubPattern()->getSemanticsProvidingPattern();
  return this;
}

} // end namespace swift

#endif
