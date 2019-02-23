//===--- DerivedConformanceTensorGroup.cpp ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements explicit derivation of the TensorGroup protocol for
// a nominal typ.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;

bool DerivedConformance::canDeriveTensorGroup(NominalTypeDecl *nominal) {
  // Nominal type must be a struct (zero stored properties is okay).
  // Note: we could extend synthesis to support classes.
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  // All stored properties must conform to `TensorGroup`.
  auto &C = nominal->getASTContext();
  auto *tensorGroupProto = C.getProtocol(KnownProtocolKind::TensorGroup);
  return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
    if (!v->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
    return (bool)TypeChecker::conformsToProtocol(varType, tensorGroupProto, DC,
                                                 ConformanceCheckFlags::Used);
  });
}

/// Derive the body for the '_typeList' getter.
static void
deriveBodyTensorGroup_typeList(AbstractFunctionDecl *hashValueDecl) {
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *tensorGroupProto = C.getProtocol(KnownProtocolKind::TensorGroup);
  auto *typesListReq = getProtocolRequirement(, C.Id_typesList);

  // Collect all member `_typeList` arrays.
  llvm::SmallVector<Expr *, 2> typeListExprs;
  for (auto member : nominal->getStoredProperties()) {
    auto *typeListExpr = new (C) 
        MemberRefExpr(member, SourceLoc(), typesListReq,
                      DeclNameLoc(), /*Implicit*/ true);
    typeListExprs.push_back(typeListExpr);
  }

  // Concatenate all arrays.
  Type arrayType = getArrayType(C, 
      C.getTensorDataTypeDecl()->getDeclaredInterfaceType());
  auto *arrayTypeExpr = TypeExpr::createImplicit(arrayType, C);
  auto plusOpLookup = C.getArrayDecl()->lookupDirect(C.getIdentifier("+"));
  assert(plusOpLookup.size() == 1 && "Ambiguous 'Array.+' operator.");
  ValueDecl *plusOpDecl = plusOpLookup.front();
  auto plusOpDRE = new (C) 
      DeclRefExpr(plusOpDecl, DeclNameLoc(), /*Implicit*/ true);
  auto plusOpExpr = 
        new (C) DotSyntaxCallExpr(plusOpDRE, SourceLoc(), arrayTypeExpr);

  auto array_add_fold = [](Expr* lhsArg, Expr* rhsArg) {
    // Create expression `lhsArg + rhsArg`.
    auto *plusOpArgs =
        TupleExpr::create(C, SourceLoc(), {lhsArg, rhsArg}, {}, {}, SourceLoc(),
                          /*HasTrailingClosure*/ false,
                          /*Implicit*/ true);
    return new (C) BinaryExpr(plusOpExpr, plusOpArgs, /*Implicit*/ true);
  };

  auto typeListExpr = std::reduce(std::next(typeListExprs.begin()), 
                                  typeListExprs.end(),
                                  typeListExprs[0],
                                  array_add_fold);

  // Return the resulting data types array.
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), typeListExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                 /*Implicit*/ true);
  funcDecl->setBody(BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true));
}

/// Derive a '_typeList' implementation.
static ValueDecl *deriveTensorGroup_typeList(DerivedConformance &derived) {
  auto &tc = derived.TC;
  ASTContext &C = tc.Context;

  auto parentDC = derived.getConformanceContext();
  Type dataTypeArrayType = getArrayType(C, 
      C.getTensorDataTypeDecl()->getDeclaredInterfaceType());

  // Create `_typeList` property declaration.
  VarDecl *typeListDecl;
  PatternBindingDecl *patDecl;
  std::tie(typeListDecl, patDecl) = derived.declareDerivedProperty(
      C.Id_typeList, dataTypeArrayType, dataTypeArrayType, /*isStatic*/ true,
      /*isFinal*/ false);
  // TODO: What's the difference between interface type and context type?

  VarDecl *typeListDecl =
    new (C) VarDecl(/*IsStatic*/ true, VarDecl::Specifier::Var,
                    /*IsCaptureList*/ false, SourceLoc(),
                    C.Id_typeList, parentDC);
  typeListDecl->setType(dataTypeArrayType);

  ParameterList *params = ParameterList::createEmpty(C);

  AccessorDecl *getterDecl = AccessorDecl::create(C,
      /*FuncLoc*/ SourceLoc(), /*AccessorKeywordLoc*/ SourceLoc(),
      AccessorKind::Get, typeListDecl,
      /*StaticLoc*/ SourceLoc(), StaticSpellingKind::None,
      /*Throws*/ false, /*ThrowsLoc*/ SourceLoc(),
      /*GenericParams*/ nullptr, params,
      TypeLoc::withoutLoc(dataTypeArrayType), parentDC);
  getterDecl->setImplicit();
  getterDecl->setBodySynthesizer(&deriveBodyTensorGroup_typeList);

  // Compute the interface type of _typeList().
  if (auto env = parentDC->getGenericEnvironmentOfContext())
    getterDecl->setGenericEnvironment(env);
  getterDecl->computeType();

  getterDecl->setValidationToChecked();
  getterDecl->copyFormalAccessFrom(derived.Nominal,
                                   /*sourceIsParentContext*/ true);

  // Finish creating the property.
  typeListDecl->setImplicit();
  typeListDecl->setInterfaceType(dataTypeArrayType);
  typeListDecl->setValidationToChecked();
  typeListDecl->setAccessors(StorageImplInfo::getImmutableComputed(),
                             SourceLoc(), {getterDecl}, SourceLoc());
  typeListDecl->copyFormalAccessFrom(derived.Nominal,
                                     /*sourceIsParentContext*/ true);

  Pattern *typeListPat = new (C) NamedPattern(typeListDecl, /*implicit*/ true);
  typeListPat->setType(dataTypeArrayType);
  typeListPat = TypedPattern::createImplicit(C, typeListPat, dataTypeArrayType);
  typeListPat->setType(dataTypeArrayType);

  C.addSynthesizedDecl(typeListDecl);
  C.addSynthesizedDecl(getterDecl);

  derived.addMembersToConformanceContext({getterDecl, typeListDecl, patDecl});
  return typeListDecl;
}

ValueDecl *DerivedConformance::deriveTensorGroup(ValueDecl *requirement) {
  ASTContext &C = ConformanceDecl->getASTContext();

  // static var _typeList: [TensorDataType]
  if (requirement->getBaseName() == C.Id_typeList) {
    return deriveTensorGroup_typeList(*this);
  }
}
