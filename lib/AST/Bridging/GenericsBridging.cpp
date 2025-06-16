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
  case RequirementReprKind::TypeConstraint:
    return RequirementRepr::getTypeConstraint(
        FirstType.unbridged(), SeparatorLoc.unbridged(), SecondType.unbridged(),
        IsExpansionPattern);
  case RequirementReprKind::SameType:
    return RequirementRepr::getSameType(
        FirstType.unbridged(), SeparatorLoc.unbridged(), SecondType.unbridged(),
        IsExpansionPattern);
  case RequirementReprKind::LayoutConstraint:
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
      /*Kind=*/RequirementReprKind::TypeConstraint,
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
      /*Kind=*/RequirementReprKind::SameType,
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
      /*Kind=*/RequirementReprKind::LayoutConstraint,
      /*FirstType=*/cSubject,
      /*SecondType=*/nullptr,
      /*LayoutConstraint=*/cLayout,
      /*LayoutConstraintLoc=*/cLayoutLoc,
      /*IsExpansionPattern=*/isExpansionPattern,
  };
}

BridgedLayoutConstraint
BridgedLayoutConstraint_getLayoutConstraint(BridgedASTContext cContext,
                                            BridgedIdentifier cID) {
  return swift::getLayoutConstraint(cID.unbridged(), cContext.unbridged());
}

BridgedLayoutConstraint
BridgedLayoutConstraint_getLayoutConstraint(BridgedASTContext cContext,
                                            swift::LayoutConstraintKind kind) {
  return LayoutConstraint::getLayoutConstraint(kind, cContext.unbridged());
}

BridgedLayoutConstraint
BridgedLayoutConstraint_getLayoutConstraint(BridgedASTContext cContext,
                                            swift::LayoutConstraintKind kind,
                                            size_t size, size_t alignment) {
  return LayoutConstraint::getLayoutConstraint(kind, size, alignment,
                                               cContext.unbridged());
}

swift::LayoutConstraintKind BridgedLayoutConstraint::getKind() const {
  return unbridged()->getKind();
}
