//===--- DerivedConformanceTensorGroup.cpp --------------------------------===//
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
// a nominal type.
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

bool DerivedConformance::canDeriveTensorGroup(NominalTypeDecl *nominal, 
                                              DeclContext *DC) {
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

// Return the protocol requirement with the specified name.
static ValueDecl *getProtocolRequirement(ProtocolDecl *proto, Identifier name) {
  auto lookup = proto->lookupDirect(name);
  lookup.erase(std::remove_if(lookup.begin(), lookup.end(),
                              [](ValueDecl *v) {
                                return !isa<ProtocolDecl>(
                                           v->getDeclContext()) ||
                                       !v->isProtocolRequirement();
                              }),
               lookup.end());
  assert(lookup.size() == 1 && "Ambiguous protocol requirement");
  return lookup.front();
}

/// Derive the body for the '_typeList' getter.
static void
deriveBodyTensorGroup_typeList(AbstractFunctionDecl *funcDecl) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *tensorGroupProto = C.getProtocol(KnownProtocolKind::TensorGroup);
  auto *typeListReq = getProtocolRequirement(tensorGroupProto, C.Id_typeList);

  // Concatenate all member `_typeList` arrays..
  Type arrayType = BoundGenericType::get(
    C.getArrayDecl(), Type(), 
    {C.getTensorDataTypeDecl()->getDeclaredInterfaceType()});
  auto *arrayTypeExpr = TypeExpr::createImplicit(arrayType, C);
  auto plusOpLookup = C.getArrayDecl()->lookupDirect(C.getIdentifier("+"));
  assert(plusOpLookup.size() == 1 && "Ambiguous 'Array.+' operator.");
  ValueDecl *plusOpDecl = plusOpLookup.front();
  auto plusOpDRE = new (C) 
      DeclRefExpr(plusOpDecl, DeclNameLoc(), /*Implicit*/ true);
  auto plusOpExpr = 
        new (C) DotSyntaxCallExpr(plusOpDRE, SourceLoc(), arrayTypeExpr);
  Expr *typeListExpr = nullptr;
  for (auto member : nominal->getStoredProperties()) {
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto *memberTypeListExpr = new (C) 
        MemberRefExpr(memberTypeExpr, SourceLoc(), typeListReq,
                      DeclNameLoc(), /*Implicit*/ true);
    if (typeListExpr == nullptr)  {
      typeListExpr = memberTypeListExpr;
    } else {
      // Create expression `lhsArg + rhsArg`.
    auto *plusOpArgs =
        TupleExpr::create(C, SourceLoc(), {typeListExpr, memberTypeListExpr}, 
                          {}, {}, SourceLoc(), /*HasTrailingClosure*/ false,
                          /*Implicit*/ true);
    typeListExpr = new (C) BinaryExpr(plusOpExpr, plusOpArgs, 
                                      /*Implicit*/ true);
    }
  }

  // Return the resulting data types array.
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), typeListExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                 /*Implicit*/ true);
  funcDecl->setBody(BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true));
}

/// Derive a '_typeList' implementation.
static ValueDecl *deriveTensorGroup_typeList(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  ASTContext &C = TC.Context;

  auto parentDC = derived.getConformanceContext();
  Type dataTypeArrayType = BoundGenericType::get(
    C.getArrayDecl(), Type(), 
    {C.getTensorDataTypeDecl()->getDeclaredInterfaceType()});
  auto returnType = parentDC->mapTypeIntoContext(dataTypeArrayType);

  // Create `_typeList` property declaration.
  VarDecl *typeListDecl;
  PatternBindingDecl *patDecl;
  std::tie(typeListDecl, patDecl) = derived.declareDerivedProperty(
      C.Id_typeList, returnType, returnType, /*isStatic*/ true,
      /*isFinal*/ false);

  // Add `@inlinable` to the `_typeList` declaration.
  if (nominal->getEffectiveAccess() > AccessLevel::Internal)
    typeListDecl->getAttrs().add(new (C) InlinableAttr(/*implicit*/ true));

  // Create `_typeList` getter.
  auto *getterDecl = derived.declareDerivedPropertyGetter(
      TC, typeListDecl, returnType);
  getterDecl->setBodySynthesizer(deriveBodyTensorGroup_typeList);
  typeListDecl->setAccessors(StorageImplInfo::getImmutableComputed(),
                                SourceLoc(), {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext({getterDecl, typeListDecl, patDecl});

  return typeListDecl;
}

/// Derive the body for the '_unknownShapeList' getter.
static void
deriveBodyTensorGroup_unknownShapeList(AbstractFunctionDecl *funcDecl) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *tensorGroupProto = C.getProtocol(KnownProtocolKind::TensorGroup);
  auto *shapeListReq = getProtocolRequirement(
      tensorGroupProto, C.Id_unknownShapeList);

  // Concatenate all member `_unknownShapeList` arrays..
  Type arrayType = BoundGenericType::get(
    C.getArrayDecl(), Type(), 
    {BoundGenericType::get(
        C.getOptionalDecl(), Type(), 
        {C.getTensorShapeDecl()->getDeclaredInterfaceType()})});
  auto *arrayTypeExpr = TypeExpr::createImplicit(arrayType, C);
  auto plusOpLookup = C.getArrayDecl()->lookupDirect(C.getIdentifier("+"));
  assert(plusOpLookup.size() == 1 && "Ambiguous 'Array.+' operator.");
  ValueDecl *plusOpDecl = plusOpLookup.front();
  auto plusOpDRE = new (C) 
      DeclRefExpr(plusOpDecl, DeclNameLoc(), /*Implicit*/ true);
  auto plusOpExpr = 
        new (C) DotSyntaxCallExpr(plusOpDRE, SourceLoc(), arrayTypeExpr);
  Expr *shapeListExpr = nullptr;
  for (auto member : nominal->getStoredProperties()) {
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto *memberShapeListExpr = new (C) 
        MemberRefExpr(memberTypeExpr, SourceLoc(), shapeListReq,
                      DeclNameLoc(), /*Implicit*/ true);
    if (shapeListExpr == nullptr)  {
      shapeListExpr = memberShapeListExpr;
    } else {
      // Create expression `lhsArg + rhsArg`.
    auto *plusOpArgs =
        TupleExpr::create(C, SourceLoc(), {shapeListExpr, memberShapeListExpr}, 
                          {}, {}, SourceLoc(), /*HasTrailingClosure*/ false,
                          /*Implicit*/ true);
    shapeListExpr = new (C) BinaryExpr(plusOpExpr, plusOpArgs, 
                                      /*Implicit*/ true);
    }
  }

  // Return the resulting data types array.
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), shapeListExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                 /*Implicit*/ true);
  funcDecl->setBody(BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true));
}

/// Derive a '_unknownShapeList' implementation.
static ValueDecl *deriveTensorGroup_unknownShapeList(
    DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  ASTContext &C = TC.Context;

  auto parentDC = derived.getConformanceContext();
  Type shapeArrayType = BoundGenericType::get(
    C.getArrayDecl(), Type(), 
    {BoundGenericType::get(
        C.getOptionalDecl(), Type(), 
        {C.getTensorShapeDecl()->getDeclaredInterfaceType()})});
  auto returnType = parentDC->mapTypeIntoContext(shapeArrayType);

  // Create `_unknownShapeList` property declaration.
  VarDecl *unknownShapeListDecl;
  PatternBindingDecl *patDecl;
  std::tie(unknownShapeListDecl, patDecl) = derived.declareDerivedProperty(
      C.Id_unknownShapeList, returnType, returnType, /*isStatic*/ true,
      /*isFinal*/ false);

  // Add `@inlinable` to the `_unknownShapeListDecl` declaration.
  if (nominal->getEffectiveAccess() > AccessLevel::Internal)
    unknownShapeListDecl->getAttrs().add(
        new (C) InlinableAttr(/*implicit*/ true));

  // Create `_unknownShapeListDecl` getter.
  auto *getterDecl = derived.declareDerivedPropertyGetter(
      TC, unknownShapeListDecl, returnType);
  getterDecl->setBodySynthesizer(deriveBodyTensorGroup_unknownShapeList);
  unknownShapeListDecl->setAccessors(StorageImplInfo::getImmutableComputed(),
                                     SourceLoc(), {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext(
      {getterDecl, unknownShapeListDecl, patDecl});

  return unknownShapeListDecl;
}

ValueDecl *DerivedConformance::deriveTensorGroup(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_typeList)
    return deriveTensorGroup_typeList(*this);
  if (requirement->getBaseName() == TC.Context.Id_unknownShapeList)
    return deriveTensorGroup_unknownShapeList(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_tensor_group_requirement);
  return nullptr;
}
