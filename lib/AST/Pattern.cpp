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

/// Diagnostic printing of PatternKinds.
llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS, PatternKind kind) {
  switch (kind) {
  case PatternKind::Paren:
    return OS << "parethesized pattern";
  case PatternKind::Tuple:
    return OS << "tuple pattern";
  case PatternKind::Named:
    return OS << "pattern variable binding";
  case PatternKind::Any:
    return OS << "'_' pattern";
  case PatternKind::Typed:
    return OS << "pattern type annotation";
  case PatternKind::Isa:
    return OS << "prefix 'is' pattern";
  case PatternKind::NominalType:
    return OS << "type destructuring pattern";
  case PatternKind::Expr:
    return OS << "expression pattern";
  case PatternKind::Var:
    return OS << "'var' binding pattern";
  case PatternKind::EnumElement:
    return OS << "enum case matching pattern";
  }
}

StringRef Pattern::getKindName(PatternKind K) {
  switch (K) {
#define PATTERN(Id, Parent) case PatternKind::Id: return #Id;
#include "swift/AST/PatternNodes.def"
  }
}

// Metaprogram to verify that every concrete class implements
// a 'static bool classof(const Pattern*)'.
template <bool fn(const Pattern*)> struct CheckClassOfPattern {
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
  
  case PatternKind::NominalType: {
    auto ntp = cast<NominalTypePattern>(this);
      
    for (auto elt : ntp->getElements()) {
      elt.getSubPattern()->collectVariables(variables);
    }
    return;
  }
      
  case PatternKind::EnumElement: {
    auto *OP = cast<EnumElementPattern>(this);
    if (OP->hasSubPattern())
      OP->collectVariables(variables);
    return;
  }
  
  case PatternKind::Expr:
    return;
      
  case PatternKind::Var:
    return cast<VarPattern>(this)->getSubPattern()->collectVariables(variables);
  }
}

Pattern *Pattern::clone(ASTContext &context, bool Implicit) const {
  Pattern *result;
  switch (getKind()) {
  case PatternKind::Any:
    result = new (context) AnyPattern(cast<AnyPattern>(this)->getLoc());
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(this);
    VarDecl *var = new (context) VarDecl(!named->getDecl()->isInstanceMember(),
                                         named->getDecl()->isLet(),
                                         named->getLoc(),
                                         named->getBoundName(),
                                         named->getDecl()->hasType()
                                           ? named->getDecl()->getType()
                                           : Type(),
                                         named->getDecl()->getDeclContext());
    if (Implicit || var->isImplicit())
      var->setImplicit();
    result = new (context) NamedPattern(var);
    break;
  }

  case PatternKind::Paren: {
    auto paren = cast<ParenPattern>(this);
    result = new (context) ParenPattern(paren->getLParenLoc(),
                                        paren->getSubPattern()->clone(context,
                                                                      Implicit),
                                        paren->getRParenLoc());
    break;
  }

  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(this);
    SmallVector<TuplePatternElt, 2> elts;
    elts.reserve(tuple->getNumFields());
    for (const auto &elt : tuple->getFields())
      elts.push_back(TuplePatternElt(elt.getPattern()->clone(context, Implicit),
                                     elt.getInit(),
                                     elt.getDefaultArgKind()));
    result = TuplePattern::create(context, tuple->getLParenLoc(), elts,
                                  tuple->getRParenLoc(),
                                  tuple->hasVararg(),
                                  tuple->getEllipsisLoc());
    break;
  }

  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(this);
    result = new(context) TypedPattern(typed->getSubPattern()->clone(context,
                                                                     Implicit),
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
    SmallVector<NominalTypePattern::Element, 4> elts;
    for (const auto &elt : nom->getElements()) {
      elts.push_back(NominalTypePattern::Element(elt.getPropertyLoc(),
                                         elt.getPropertyName(),
                                         elt.getProperty(),
                                         elt.getColonLoc(),
                                         elt.getSubPattern()->clone(context,
                                                                    Implicit)));
    }
    
    result = NominalTypePattern::create(nom->getCastTypeLoc(),
                                        nom->getLParenLoc(),
                                        elts,
                                        nom->getRParenLoc(), context);
    break;
  }
      
  case PatternKind::EnumElement: {
    auto oof = cast<EnumElementPattern>(this);
    Pattern *sub = nullptr;
    if (oof->hasSubPattern())
      sub = oof->getSubPattern()->clone(context, Implicit);
    result = new(context) EnumElementPattern(oof->getParentType(),
                                              oof->getLoc(),
                                              oof->getNameLoc(),
                                              oof->getName(),
                                              oof->getElementDecl(),
                                              sub);
    break;
  }
      
  case PatternKind::Expr: {
    auto expr = cast<ExprPattern>(this);
    result = new(context) ExprPattern(expr->getSubExpr(),
                                      expr->isResolved(),
                                      expr->getMatchExpr(),
                                      expr->getMatchVar());
    break;
  }
      
  case PatternKind::Var: {
    auto var = cast<VarPattern>(this);
    result = new(context) VarPattern(var->getLoc(),
                                     var->getSubPattern()->clone(context,
                                                                 Implicit));
  }
  }

  if (hasType())
    result->setType(getType());
  if (Implicit || isImplicit())
    result->setImplicit();

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

/// Allocate a new pattern that matches a tuple.
TuplePattern *TuplePattern::create(ASTContext &C, SourceLoc lp,
                                   ArrayRef<TuplePatternElt> elts, SourceLoc rp,
                                   bool hasVararg, SourceLoc ellipsis,
                                   Optional<bool> implicit) {
  if (!implicit.hasValue())
    implicit = !lp.isValid();

  unsigned n = elts.size();
  void *buffer = C.Allocate(sizeof(TuplePattern) + n * sizeof(TuplePatternElt) +
                            (hasVararg ? sizeof(SourceLoc) : 0),
                            alignof(TuplePattern));
  TuplePattern *pattern = ::new(buffer) TuplePattern(lp, n, rp, hasVararg,
                                                     ellipsis, *implicit);
  memcpy(pattern->getFieldsBuffer(), elts.data(), n * sizeof(TuplePatternElt));
  return pattern;
}

Pattern *TuplePattern::createSimple(ASTContext &C, SourceLoc lp,
                                    ArrayRef<TuplePatternElt> elements,
                                    SourceLoc rp,
                                    bool hasVararg, SourceLoc ellipsis) {
  assert(lp.isValid() == rp.isValid());

  if (elements.size() == 1 &&
      elements[0].getInit() == nullptr &&
      elements[0].getPattern()->getBoundName().empty() &&
      !hasVararg) {
    auto &first = const_cast<TuplePatternElt&>(elements.front());
    return new (C) ParenPattern(lp, first.getPattern(), rp);
  }

  return create(C, lp, elements, rp, hasVararg, ellipsis);
}

SourceRange TuplePattern::getSourceRange() const {
  if (LPLoc.isValid())
    return { LPLoc, RPLoc };
  auto Fields = getFields();
  if (Fields.empty())
    return {};
  return { Fields.front().getPattern()->getStartLoc(),
           Fields.back().getPattern()->getEndLoc() };
}

SourceRange TypedPattern::getSourceRange() const {
  if (isImplicit()) {
    // If a TypedPattern is implicit, then its type is definitely implicit, se
    // we should ignore its location.  On the other hand, the sub-pattern can
    // be explicit or implicit.
    return SubPattern->getSourceRange();
  }
  return { SubPattern->getSourceRange().Start, PatType.getSourceRange().End };
}

NominalTypePattern *NominalTypePattern::create(TypeLoc CastTy,
                                               SourceLoc LParenLoc,
                                               ArrayRef<Element> Elements,
                                               SourceLoc RParenLoc,
                                               ASTContext &C,
                                               Optional<bool> implicit) {
  void *buf = C.Allocate(sizeof(NominalTypePattern)
                           + sizeof(Element) * Elements.size(),
                         alignof(Element));
  return ::new (buf) NominalTypePattern(CastTy, LParenLoc, Elements, RParenLoc,
                                        implicit);
}
