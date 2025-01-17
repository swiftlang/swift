//===--- Bridging/GenericsBridging.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
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
  for (auto &cReq : cRequirements.unbridged<BridgedRequirementRepr>()) {
    switch (cReq.Kind) {
    case BridgedRequirementReprKindTypeConstraint:
      requirements.push_back(RequirementRepr::getTypeConstraint(
          cReq.FirstType.unbridged(), cReq.SeparatorLoc.unbridged(),
          cReq.SecondType.unbridged(),
          /*isExpansionPattern*/ false));
      break;
    case BridgedRequirementReprKindSameType:
      requirements.push_back(RequirementRepr::getSameType(
          cReq.FirstType.unbridged(), cReq.SeparatorLoc.unbridged(),
          cReq.SecondType.unbridged(),
          /*isExpansionPattern*/ false));
      break;
    case BridgedRequirementReprKindLayoutConstraint:
      llvm_unreachable("cannot handle layout constraints!");
    }
  }

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
