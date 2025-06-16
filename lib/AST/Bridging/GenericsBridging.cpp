//===--- Bridging/GenericsBridging.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Initializer.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Generics
//===----------------------------------------------------------------------===//

BridgedGenericParamList BridgedGenericParamList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftAngleLoc,
    BridgedArrayRef cParameters,
    BridgedNullableTrailingWhereClause bridgedGenericWhereClause,
    BridgedSourceLoc cRightAngleLoc) {
  SourceLoc whereLoc;
  ArrayRef<RequirementRepr> requirements;
  if (auto *genericWhereClause = bridgedGenericWhereClause.unbridged()) {
    whereLoc = genericWhereClause->getWhereLoc();
    requirements = genericWhereClause->getRequirements();
  }

  return GenericParamList::create(
      cContext.unbridged(), cLeftAngleLoc.unbridged(),
      cParameters.unbridged<GenericTypeParamDecl *>(), whereLoc, requirements,
      cRightAngleLoc.unbridged());
}

BridgedGenericTypeParamDecl BridgedGenericTypeParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableTypeRepr bridgedInheritedType,
    size_t index, BridgedGenericTypeParamKind cParamKind) {
  auto specifierLoc = cSpecifierLoc.unbridged();

  GenericTypeParamKind paramKind;

  switch (cParamKind) {
  case BridgedGenericTypeParamKindType:
    paramKind = GenericTypeParamKind::Type;
    break;
  case BridgedGenericTypeParamKindPack:
    paramKind = GenericTypeParamKind::Pack;
    break;
  case BridgedGenericTypeParamKindValue:
    paramKind = GenericTypeParamKind::Value;
    break;
  }

  auto *decl = GenericTypeParamDecl::createParsed(
      cDeclContext.unbridged(), cName.unbridged(), cNameLoc.unbridged(),
      specifierLoc, index, paramKind);

  if (auto *inheritedType = bridgedInheritedType.unbridged()) {
    auto entry = InheritedEntry(inheritedType);
    ASTContext &context = cContext.unbridged();
    decl->setInherited(context.AllocateCopy(llvm::ArrayRef(entry)));
  }

  return decl;
}

BridgedTrailingWhereClause
BridgedTrailingWhereClause_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cWhereKeywordLoc,
                                        BridgedArrayRef cRequirements) {
  SmallVector<RequirementRepr> requirements;
  for (auto &cReq : cRequirements.unbridged<BridgedRequirementRepr>())
    requirements.push_back(cReq.unbridged());

  SourceLoc whereKeywordLoc = cWhereKeywordLoc.unbridged();
  SourceLoc endLoc;
  if (requirements.empty()) {
    endLoc = whereKeywordLoc;
  } else {
    endLoc = requirements.back().getSourceRange().End;
  }

  return TrailingWhereClause::create(cContext.unbridged(), whereKeywordLoc,
                                     endLoc, requirements);
}

RequirementRepr BridgedRequirementRepr::unbridged() const {
  switch (Kind) {
  case BridgedRequirementReprKindTypeConstraint:
    return RequirementRepr::getTypeConstraint(
        FirstType.unbridged(), SeparatorLoc.unbridged(), SecondType.unbridged(),
        IsExpansionPattern);
  case BridgedRequirementReprKindSameType:
    return RequirementRepr::getSameType(
        FirstType.unbridged(), SeparatorLoc.unbridged(), SecondType.unbridged(),
        IsExpansionPattern);
  case BridgedRequirementReprKindLayoutConstraint:
    return RequirementRepr::getLayoutConstraint(
        FirstType.unbridged(), SeparatorLoc.unbridged(),
        {LayoutConstraint.unbridged(), LayoutConstraintLoc.unbridged()},
        IsExpansionPattern);
  }
}

BridgedRequirementRepr BridgedRequirementRepr_createTypeConstraint(
    BridgedTypeRepr cSubject, BridgedSourceLoc cColonLoc,
    BridgedTypeRepr cConstraint, bool isExpansionPattern) {
  return {
      /*SeparatorLoc=*/cColonLoc,
      /*Kind=*/BridgedRequirementReprKindTypeConstraint,
      /*FirstType=*/cSubject,
      /*SecondType=*/cConstraint.unbridged(),
      /*LayoutConstraint=*/{},
      /*LayoutConstraintLoc=*/{},
      /*IsExpansionPattern=*/isExpansionPattern,
  };
}

BridgedRequirementRepr BridgedRequirementRepr_createSameType(
    BridgedTypeRepr cFirstType, BridgedSourceLoc cEqualLoc,
    BridgedTypeRepr cSecondType, bool isExpansionPattern) {
  return {
      /*SeparatorLoc=*/cEqualLoc,
      /*Kind=*/BridgedRequirementReprKindSameType,
      /*FirstType=*/cFirstType,
      /*SecondType=*/cSecondType.unbridged(),
      /*LayoutConstraint=*/{},
      /*LayoutConstraintLoc=*/{},
      /*IsExpansionPattern=*/isExpansionPattern,
  };
}

BridgedRequirementRepr BridgedRequirementRepr_createLayoutConstraint(
    BridgedTypeRepr cSubject, BridgedSourceLoc cColonLoc,
    BridgedLayoutConstraint cLayout, BridgedSourceLoc cLayoutLoc,
    bool isExpansionPattern) {
  return {
      /*SeparatorLoc=*/cColonLoc,
      /*Kind=*/BridgedRequirementReprKindLayoutConstraint,
      /*FirstType=*/cSubject,
      /*SecondType=*/nullptr,
      /*LayoutConstraint=*/cLayout,
      /*LayoutConstraintLoc=*/cLayoutLoc,
      /*IsExpansionPattern=*/isExpansionPattern,
  };
}

static swift::LayoutConstraintKind
unbridged(BridgedLayoutConstraintKind cKind) {
  switch (cKind) {
#define CASE(Kind)                                                             \
  case BridgedLayoutConstraintKind##Kind:                                      \
    return swift::LayoutConstraintKind::Kind;
    CASE(UnknownLayout)
    CASE(TrivialOfExactSize)
    CASE(TrivialOfAtMostSize)
    CASE(Trivial)
    CASE(Class)
    CASE(NativeClass)
    CASE(RefCountedObject)
    CASE(NativeRefCountedObject)
    CASE(BridgeObject)
    CASE(TrivialStride)
#undef CASE
  }
}

static BridgedLayoutConstraintKind bridge(swift::LayoutConstraintKind kind) {
  switch (kind) {
#define CASE(Kind)                                                             \
  case swift::LayoutConstraintKind::Kind:                                      \
    return BridgedLayoutConstraintKind##Kind;
    CASE(UnknownLayout)
    CASE(TrivialOfExactSize)
    CASE(TrivialOfAtMostSize)
    CASE(Trivial)
    CASE(Class)
    CASE(NativeClass)
    CASE(RefCountedObject)
    CASE(NativeRefCountedObject)
    CASE(BridgeObject)
    CASE(TrivialStride)
#undef CASE
  }
}

BridgedLayoutConstraint
BridgedLayoutConstraint_getLayoutConstraint(BridgedASTContext cContext,
                                            BridgedIdentifier cID) {
  return swift::getLayoutConstraint(cID.unbridged(), cContext.unbridged());
}

BridgedLayoutConstraint
BridgedLayoutConstraint_getLayoutConstraint(BridgedASTContext cContext,
                                            BridgedLayoutConstraintKind cKind) {
  return LayoutConstraint::getLayoutConstraint(unbridged(cKind),
                                               cContext.unbridged());
}

BridgedLayoutConstraint
BridgedLayoutConstraint_getLayoutConstraint(BridgedASTContext cContext,
                                            BridgedLayoutConstraintKind cKind,
                                            size_t size, size_t alignment) {
  return LayoutConstraint::getLayoutConstraint(unbridged(cKind), size,
                                               alignment, cContext.unbridged());
}

BridgedLayoutConstraintKind BridgedLayoutConstraint::getKind() const {
  return bridge(unbridged()->getKind());
}
