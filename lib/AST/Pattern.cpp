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
  forEachVariable([&](VarDecl *VD) { variables.push_back(VD); });
}

VarDecl *Pattern::getSingleVar() const {
  auto pattern = getSemanticsProvidingPattern();
  if (auto named = dyn_cast<NamedPattern>(pattern))
    return named->getDecl();

  return nullptr;
}

/// \brief apply the specified function to all variables referenced in this
/// pattern.
void Pattern::forEachVariable(const std::function<void(VarDecl*)> &fn) const {
  switch (getKind()) {
  case PatternKind::Any:
  case PatternKind::Isa:
  case PatternKind::Expr:
    return;

  case PatternKind::Named:
    fn(cast<NamedPattern>(this)->getDecl());
    return;

  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    return getSemanticsProvidingPattern()->forEachVariable(fn);

  case PatternKind::Tuple:
    for (auto elt : cast<TuplePattern>(this)->getFields())
      elt.getPattern()->forEachVariable(fn);
    return;

  case PatternKind::NominalType:
    for (auto elt : cast<NominalTypePattern>(this)->getElements())
      elt.getSubPattern()->forEachVariable(fn);
    return;

  case PatternKind::EnumElement: {
    auto *OP = cast<EnumElementPattern>(this);
    if (OP->hasSubPattern())
      OP->getSubPattern()->forEachVariable(fn);
    return;
  }
  }
}

/// \brief apply the specified function to all pattern nodes recursively in
/// this pattern.  This is a pre-order traversal.
void Pattern::forEachNode(const std::function<void(Pattern*)> &f) {
  f(this);

  switch (getKind()) {
  // Leaf patterns have no recursion.
  case PatternKind::Any:
  case PatternKind::Named:
  case PatternKind::Isa:
  case PatternKind::Expr:// FIXME: expr nodes are not modeled right in general.
    return;

  case PatternKind::Paren:
    return cast<ParenPattern>(this)->getSubPattern()->forEachNode(f);
  case PatternKind::Typed:
    return cast<TypedPattern>(this)->getSubPattern()->forEachNode(f);
  case PatternKind::Var:
    return cast<VarPattern>(this)->getSubPattern()->forEachNode(f);

  case PatternKind::Tuple:
    for (auto elt : cast<TuplePattern>(this)->getFields())
      elt.getPattern()->forEachNode(f);
    return;

  case PatternKind::NominalType:
    for (auto elt : cast<NominalTypePattern>(this)->getElements())
      elt.getSubPattern()->forEachNode(f);
    return;

  case PatternKind::EnumElement: {
    auto *OP = cast<EnumElementPattern>(this);
    if (OP->hasSubPattern())
      OP->getSubPattern()->forEachNode(f);
    return;
  }
  }
}

unsigned Pattern::numTopLevelVariables() const {
  auto pattern = getSemanticsProvidingPattern();
  if (auto tuple = dyn_cast<TuplePattern>(pattern))
    return tuple->getNumFields();
  return 1;
}

Pattern *Pattern::clone(ASTContext &context,
                        OptionSet<CloneFlags> options) const {
  Pattern *result;
  switch (getKind()) {
  case PatternKind::Any: {
    auto any = cast<AnyPattern>(this);
    if (options & AlwaysNamed) {
      VarDecl *var = new (context) VarDecl(/*isStatic=*/false,
                                           !(options & Pattern::IsVar),
                                           any->getLoc(),
                                           context.getIdentifier("_"),
                                           any->hasType()? any->getType()
                                                         : Type(),
                                           nullptr);
      if (options & Implicit)
        var->setImplicit();

      result = new (context) NamedPattern(var);
    } else {
      result = new (context) AnyPattern(cast<AnyPattern>(this)->getLoc());
    }
    break;
  }

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(this);
    VarDecl *var;
    if (auto param = dyn_cast<ParamDecl>(named->getDecl())) {
      var = new (context) ParamDecl(param->isLet(),
                                    param->getArgumentNameLoc(),
                                    param->getArgumentName(),
                                    param->getLoc(),
                                    param->getName(),
                                    param->hasType()
                                      ? param->getType()
                                      : Type(),
                                    param->getDeclContext());
    } else {
      var = new (context) VarDecl(!named->getDecl()->isInstanceMember(),
                                  named->getDecl()->isLet(),
                                  named->getLoc(),
                                  named->getBoundName(),
                                  named->getDecl()->hasType()
                                    ? named->getDecl()->getType()
                                    : Type(),
                                  named->getDecl()->getDeclContext());
    }

    if ((options & Implicit) || var->isImplicit())
      var->setImplicit();
    result = new (context) NamedPattern(var);
    break;
  }

  case PatternKind::Paren: {
    auto paren = cast<ParenPattern>(this);
    result = new (context) ParenPattern(paren->getLParenLoc(),
                                        paren->getSubPattern()->clone(context,
                                                                      options),
                                        paren->getRParenLoc());
    break;
  }

  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(this);
    SmallVector<TuplePatternElt, 2> elts;
    elts.reserve(tuple->getNumFields());
    for (const auto &elt : tuple->getFields()) {
      auto eltPattern = elt.getPattern()->clone(context, options);

      // If we're inheriting a default argument, mark it as such.
      if (elt.getDefaultArgKind() != DefaultArgumentKind::None &&
          (options & Inherited)) {
        elts.push_back(TuplePatternElt(eltPattern, nullptr,
                                       DefaultArgumentKind::Inherited));
      } else {
        elts.push_back(TuplePatternElt(eltPattern,
                                       elt.getInit(),
                                       elt.getDefaultArgKind()));
      }
    }

    result = TuplePattern::create(context, tuple->getLParenLoc(), elts,
                                  tuple->getRParenLoc(),
                                  tuple->hasVararg(),
                                  tuple->getEllipsisLoc());
    break;
  }

  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(this);
    result = new(context) TypedPattern(typed->getSubPattern()->clone(context,
                                                                     options),
                                       typed->getTypeLoc().clone(context));
    break;
  }
      
  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(this);
    result = new(context) IsaPattern(isa->getLoc(),
                                     isa->getCastTypeLoc().clone(context),
                                     isa->getSubPattern()->clone(context,
                                                                 options),
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
                                                                    options)));
    }
    
    result = NominalTypePattern::create(nom->getCastTypeLoc().clone(context),
                                        nom->getLParenLoc(),
                                        elts,
                                        nom->getRParenLoc(), context);
    break;
  }
      
  case PatternKind::EnumElement: {
    auto oof = cast<EnumElementPattern>(this);
    Pattern *sub = nullptr;
    if (oof->hasSubPattern())
      sub = oof->getSubPattern()->clone(context, options);
    result = new (context) EnumElementPattern(oof->getParentType()
                                                .clone(context),
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
                                     var->getSubPattern()->clone(
                                       context,
                                       options|IsVar));
  }
  }

  if (hasType())
    result->setType(getType());
  if ((options & Implicit) || isImplicit())
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
  if (auto *NP = dyn_cast<NamedPattern>(getSemanticsProvidingPattern()))
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

  if (SubPattern->isImplicit())
    return PatType.getSourceRange();

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
