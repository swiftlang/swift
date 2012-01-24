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
    unsigned NumElements : NumBitsAllocated - NumPatternBits;
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

  /// Returns whether this pattern has been type-checked yet.
  bool hasType() const { return !Ty.isNull(); }

  /// If thie pattern has been type-checked, return the type it
  /// matches.
  Type getType() const { assert(hasType()); return Ty; }

  /// Set the type of this pattern, given that it was previously not
  /// type-checked.
  void setType(Type ty) { assert(!hasType()); Ty = ty; }

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

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const { return SourceRange(LPLoc, RPLoc); }
  SourceLoc getLoc() const { return LPLoc; }

  Pattern *getSubPattern() const { return SubPattern; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Paren;
  }
  static bool classof(const ParenPattern *P) { return true; }
};

/// A pattern consisting of a tuple of patterns.
class TuplePattern : public Pattern {
  SourceLoc LPLoc, RPLoc;
  // TuplePatternBits.NumElements

  Pattern **getElementsBuffer() {
    return reinterpret_cast<Pattern**>(this+1);
  }
  Pattern * const *getElementsBuffer() const {
    return reinterpret_cast<Pattern * const *>(this + 1);
  }

  TuplePattern(SourceLoc lp, unsigned numElements, SourceLoc rp)
      : Pattern(PatternKind::Tuple), LPLoc(lp), RPLoc(rp) {
    TuplePatternBits.NumElements = numElements;
  }

public:
  TuplePattern *create(ASTContext &C, SourceLoc lp,
                       ArrayRef<Pattern*> elements, SourceLoc rp);

  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }
  SourceRange getSourceRange() const { return SourceRange(LPLoc, RPLoc); }
  SourceLoc getLoc() const { return LPLoc; }

  unsigned getNumElements() const {
    return TuplePatternBits.NumElements;
  }

  ArrayRef<Pattern*> getElements() const {
    return ArrayRef<Pattern*>(getElementsBuffer(), getNumElements());
  }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Tuple;
  }
  static bool classof(const TuplePattern *P) { return true; }
};

/// A pattern which binds a top-level name.  That is, this pattern
/// binds a name and is not contained within a pattern that also binds
/// a name.  It may still be contained within a pattern that does not
/// bind a name.
class VarPattern : public Pattern {
  VarDecl *const Var;

public:
  VarPattern(VarDecl *var) : Pattern(PatternKind::Var), Var(var) {}

  SourceLoc getLoc() const { return Var->getLocStart(); }
  SourceRange getSourceRange() const { return getLoc(); }

  VarDecl *getDecl() const { return Var; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::Var;
  }
  static bool classof(const VarPattern *P) { return true; }
};

/// A pattern which binds a name other than at the top-level.
class ElementRefPattern : public Pattern {
  ElementRefDecl *const ElementRef;

public:
  ElementRefPattern(ElementRefDecl *elementRef)
    : Pattern(PatternKind::ElementRef), ElementRef(elementRef) {}

  SourceLoc getLoc() const { return ElementRef->getLocStart(); }
  SourceRange getSourceRange() const { return getLoc(); }

  ElementRefDecl *getDecl() const { return ElementRef; }

  static bool classof(const Pattern *P) {
    return P->getKind() == PatternKind::ElementRef;
  }
  static bool classof(const ElementRefPattern *P) { return true; }
};

} // end namespace swift

#endif
