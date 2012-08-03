//===--- Pattern.cpp - Swift Language Pattern-Matching ASTs ---------------===//
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
//  This file implements the Pattern class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Pattern.h"
#include "swift/AST/AST.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

// Metaprogram to verify that every concrete class implements
// a 'static bool classof(const Pattern*)'.
template <bool (&fn)(const Pattern*)> struct CheckClassOfPattern {
  static const bool IsImplemented = true;
};
template <> struct CheckClassOfPattern<Pattern::classof> {
  static const bool IsImplemented = false;
};

#define PATTERN(ID, PARENT) \
static_assert(CheckClassOfPattern<ID##Pattern::classof>::IsImplemented, \
              #ID "Pattern is missing classof(const Pattern*)");
#include "swift/AST/PatternNodes.def"

// Metaprogram to verify that every concrete class implements
// 'SourceRange getSourceRange()'.
typedef const char (&TwoChars)[2];
template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);
inline TwoChars checkSourceRangeType(SourceRange (Pattern::*)() const);


void Pattern::setType(Type ty) {
  assert(!hasType() || getType()->isUnresolvedType());
  Ty = ty;
}


/// getSourceRange - Return the full source range of the pattern.
SourceRange Pattern::getSourceRange() const {
  switch (getKind()) {
#define PATTERN(ID, PARENT) \
case PatternKind::ID: \
static_assert(sizeof(checkSourceRangeType(&ID##Pattern::getSourceRange)) == 1, \
              #ID "Pattern is missing getSourceRange()"); \
return cast<ID##Pattern>(this)->getSourceRange();
#include "swift/AST/PatternNodes.def"
  }
  
  llvm_unreachable("pattern type not handled!");
}

/// getLoc - Return the caret location of the pattern.
SourceLoc Pattern::getLoc() const {
  switch (getKind()) {
#define PATTERN(ID, PARENT) \
  case PatternKind::ID: \
    if (&Pattern::getLoc != &ID##Pattern::getLoc) \
      return cast<ID##Pattern>(this)->getLoc(); \
    break;
#include "swift/AST/PatternNodes.def"
  }

  return getStartLoc();
}

/// Standard allocator for Patterns.
void *Pattern::operator new(size_t numBytes, ASTContext &C) {
  return C.Allocate(numBytes, Pattern::Alignment);
}

/// Find the name directly bound by this pattern.  When used as a
/// tuple element in a function signature, such names become part of
/// the type.
Identifier Pattern::getBoundName() const {
  const Pattern *P = this;
  if (const TypedPattern *TP = dyn_cast<TypedPattern>(P))
    P = TP->getSubPattern();

  if (const NamedPattern *NP = dyn_cast<NamedPattern>(P))
    return NP->getBoundName();
  return Identifier();
}

/// Allocate a new pattern that matches a tuple.
TuplePattern *TuplePattern::create(ASTContext &C, SourceLoc lp,
                                   ArrayRef<TuplePatternElt> elts,
                                   SourceLoc rp) {
  unsigned n = elts.size();
  void *buffer = C.Allocate(sizeof(TuplePattern) + n * sizeof(TuplePatternElt),
                            Pattern::Alignment);
  TuplePattern *pattern = ::new(buffer) TuplePattern(lp, n, rp);
  memcpy(pattern->getFieldsBuffer(), elts.data(), n * sizeof(TuplePatternElt));
  return pattern;
}

SourceRange TypedPattern::getSourceRange() const {
  return { SubPattern->getSourceRange().Start, PatType.getSourceRange().End };
}
