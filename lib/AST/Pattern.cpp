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
  assert(!hasType() || getType()->isUnresolvedType() ||
         ty->is<ErrorType>() ||
         ty->getWithoutDefaultArgs(ty->getASTContext())->isEqual(
           Ty->getWithoutDefaultArgs(Ty->getASTContext())));

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

void Pattern::collectVariables(SmallVectorImpl<VarDecl *> &variables) const {
  switch (getKind()) {
  case PatternKind::Any:
    return;

  case PatternKind::Named:
    variables.push_back(cast<NamedPattern>(this)->getDecl());
    return;

  case PatternKind::Paren:
    return cast<ParenPattern>(this)->getSubPattern()
             ->collectVariables(variables);

  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(this);
    for (auto elt : tuple->getFields()) {
      elt.getPattern()->collectVariables(variables);
    }
    return;
  }

  case PatternKind::Typed:
    return cast<TypedPattern>(this)->getSubPattern()
             ->collectVariables(variables);
      
  case PatternKind::Isa:
    return;
  
  case PatternKind::NominalType:
    return cast<NominalTypePattern>(this)->getSubPattern()
             ->collectVariables(variables);
  
  case PatternKind::Expr:
    return;
      
  case PatternKind::Var:
    return cast<VarPattern>(this)->getSubPattern()->collectVariables(variables);
  }
}

Pattern *Pattern::clone(ASTContext &context) const {
  Pattern *result;
  switch (getKind()) {
  case PatternKind::Any:
    result = new (context) AnyPattern(cast<AnyPattern>(this)->getLoc());
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(this);
    VarDecl *var = new (context) VarDecl(named->getLoc(),
                                         named->getBoundName(),
                                         named->getDecl()->hasType()
                                           ? named->getDecl()->getType()
                                           : Type(),
                                         named->getDecl()->getDeclContext());
    result = new (context) NamedPattern(var);
    break;
  }

  case PatternKind::Paren: {
    auto paren = cast<ParenPattern>(this);
    return new (context) ParenPattern(paren->getLParenLoc(),
                                      paren->getSubPattern()->clone(context),
                                      paren->getRParenLoc());
  }

  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(this);
    SmallVector<TuplePatternElt, 2> elts;
    elts.reserve(tuple->getNumFields());
    for (const auto &elt : tuple->getFields())
      elts.push_back(TuplePatternElt(elt.getPattern()->clone(context),
                                     elt.getInit(), elt.getVarargBaseType()));
    result = TuplePattern::create(context, tuple->getLParenLoc(), elts,
                                  tuple->getRParenLoc());
    break;
  }

  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(this);
    result = new(context) TypedPattern(typed->getSubPattern()->clone(context),
                                       typed->getTypeLoc());
    break;
  }
      
  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(this);
    result = new(context) IsaPattern(isa->getLoc(),
                                     isa->getCastTypeLoc(),
                                     isa->getCastKind());
    break;
  }
      
  case PatternKind::NominalType: {
    auto nom = cast<NominalTypePattern>(this);
    result = new(context) NominalTypePattern(nom->getCastTypeLoc(),
                                           nom->getSubPattern()->clone(context),
                                           nom->getCastKind());
    break;
  }
      
  case PatternKind::Expr: {
    auto expr = cast<ExprPattern>(this);
    result = new(context) ExprPattern(expr->getSubExpr(),
                                      expr->isResolved(),
                                      expr->getMatchFnExpr());
    break;
  }
      
  case PatternKind::Var: {
    auto var = cast<VarPattern>(this);
    result = new(context) VarPattern(var->getLoc(),
                                     var->getSubPattern()->clone(context));
  }
  }

  if (hasType())
    result->setType(getType());

  return result;
}

/// Standard allocator for Patterns.
void *Pattern::operator new(size_t numBytes, ASTContext &C) {
  return C.Allocate(numBytes, alignof(Pattern));
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

void TuplePatternElt::revertToNonVariadic() {
  assert(VarargBaseType && "Not a variadic element");

  // Fix the pattern.
  auto typedPattern = cast<TypedPattern>(ThePattern);
  typedPattern->getTypeLoc()
    = TypeLoc(VarargBaseType, typedPattern->getTypeLoc().getSourceRange());

  // Clear out the variadic base type.
  VarargBaseType = Type();
}

/// Allocate a new pattern that matches a tuple.
TuplePattern *TuplePattern::create(ASTContext &C, SourceLoc lp,
                                   ArrayRef<TuplePatternElt> elts,
                                   SourceLoc rp) {
  unsigned n = elts.size();
  void *buffer = C.Allocate(sizeof(TuplePattern) + n * sizeof(TuplePatternElt),
                            alignof(TuplePattern));
  TuplePattern *pattern = ::new(buffer) TuplePattern(lp, n, rp);
  memcpy(pattern->getFieldsBuffer(), elts.data(), n * sizeof(TuplePatternElt));
  return pattern;
}

Pattern *TuplePattern::createSimple(ASTContext &C, SourceLoc lp,
                                    ArrayRef<TuplePatternElt> elements,
                                    SourceLoc rp) {
  if (elements.size() == 1 &&
      elements[0].getInit() == nullptr &&
      elements[0].getPattern()->getBoundName().empty() &&
      !elements[0].isVararg()) {
    auto &first = const_cast<TuplePatternElt&>(elements.front());
    return new (C) ParenPattern(lp, first.getPattern(), rp);
  }

  return create(C, lp, elements, rp);
}

SourceRange TypedPattern::getSourceRange() const {
  return { SubPattern->getSourceRange().Start, PatType.getSourceRange().End };
}
