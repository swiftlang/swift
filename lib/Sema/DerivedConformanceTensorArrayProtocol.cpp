//===--- DerivedConformanceTensorArrayProtocol.cpp ------------------------===//
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
// This file implements explicit derivation of the TensorArrayProtocol protocol 
// for a nominal type.
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

bool DerivedConformance::canDeriveTensorArrayProtocol(NominalTypeDecl *nominal, 
                                                      DeclContext *DC) {
  // Nominal type must be a struct (zero stored properties is okay).
  // Note: we could extend synthesis to support classes.
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  // All stored properties must conform to `TensorArrayProtocol`.
  auto &C = nominal->getASTContext();
  auto *tensorArrayProto = 
      C.getProtocol(KnownProtocolKind::TensorArrayProtocol);
  return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
    if (!v->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
    return (bool)TypeChecker::conformsToProtocol(varType, tensorArrayProto, DC,
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

// Synthesize body for `_unpackTensorHandles(into:)`.
static void 
deriveBodyTensorArrayProtocol_unpackTensorHandles(
    AbstractFunctionDecl *funcDecl) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Obtain the address type.
  auto cTensorHandleType = C.getOpaquePointerDecl()->getDeclaredType();
  Type baseAddressType = BoundGenericType::get(
      C.getUnsafeMutablePointerDecl(), Type(), {cTensorHandleType});
  Type addressType = BoundGenericType::get(
      C.getOptionalDecl(), Type(), {baseAddressType});

  // Get the address parameter.
  auto *paramDecl = funcDecl->getParameters()->get(0);
  auto *paramDRE = new (C) 
      DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit*/ true, 
                  AccessSemantics::Ordinary, /*Ty*/ addressType);
  paramDRE->setFunctionRefKind(FunctionRefKind::Unapplied);

  // Create an `if var` statement for the current address.
  VarDecl *currAddressDecl = new (C) VarDecl(
      /*IsStatic*/ false, VarDecl::Specifier::Var, /*IsCaptureList*/ false, 
      SourceLoc(), C.getIdentifier("currentAddress"), parentDC);
  currAddressDecl->setImplicit();
  currAddressDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  currAddressDecl->setInterfaceType(baseAddressType);
  currAddressDecl->setValidationToChecked();

  Pattern *currAddressPat = new (C) 
      NamedPattern(currAddressDecl, /*implicit*/ true);
  currAddressPat->setType(baseAddressType);
  currAddressPat = new (C) 
      VarPattern(SourceLoc(), /*isLet*/ false, currAddressPat, 
                 /*implicit*/ true);
  currAddressPat->setType(baseAddressType);
  currAddressPat = new (C)
      OptionalSomePattern(currAddressPat, currAddressPat->getEndLoc(), 
                          /*implicit*/ true);
  currAddressPat->setType(addressType);
  auto cond = StmtConditionElement(SourceLoc(), currAddressPat, 
                                   /*Init*/ paramDRE);

  // Get method protocol requirement.
  auto *tensorArrayProto = C.getProtocol(
      KnownProtocolKind::TensorArrayProtocol);
  auto *methodReq = getProtocolRequirement(tensorArrayProto, 
                                           C.Id_unpackTensorHandles);
  
  Type intType = C.getIntDecl()->getDeclaredType();
  TypeExpr *intTypeExpr = TypeExpr::createImplicit(intType, C);

  // Go through the member TensorArrayProtocols and call 
  // `_unpackTensorHandles(into:)`.
  llvm::SmallVector<ASTNode, 2> memberExprs;
  for (auto member : nominal->getStoredProperties()) {
    auto *memberRefExpr = new (C) DeclRefExpr(member, DeclNameLoc(),
                                              /*Implicit*/ true);
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto *memberMethodRefExpr = new (C) 
        MemberRefExpr(memberTypeExpr, SourceLoc(), methodReq,
                      DeclNameLoc(), /*Implicit*/ true);
    auto *methodCallExpr = new (C) 
        DotSyntaxCallExpr(memberRefExpr, SourceLoc(), memberMethodRefExpr);
    
    // Obtain the method call argument.
    auto *addressDeclRefExpr = new (C) 
        DeclRefExpr(currAddressDecl, DeclNameLoc(), /*implicit*/ true);
    auto *loadExpr = new (C) LoadExpr(addressDeclRefExpr, baseAddressType);
    auto *injectExpr = new (C) InjectIntoOptionalExpr(loadExpr, addressType);
    auto *tupleExpr = TupleExpr::create(
        C, SourceLoc(), {injectExpr}, {C.getIdentifier("into")}, {}, 
        SourceLoc(), /*HasTrailingClosure*/ false, /*Implicit*/ true);

    auto *callExpr = CallExpr::createImplicit(
        C, methodCallExpr, {tupleExpr}, {C.getIdentifier("into")});
    
    // Advance the current address.
    auto *dotExpr = new (C)
        UnresolvedDotExpr(addressDeclRefExpr, SourceLoc(), 
                          C.getIdentifier("advanced"), DeclNameLoc(), 
                          /*Implicit*/ true);
    DeclName name(C, C.Id_tensorHandleCount, {Identifier()});
    auto *countDotExpr = new (C)
        UnresolvedDotExpr(memberTypeExpr, SourceLoc(), name, 
                          DeclNameLoc(), /*Implicit*/ true);
    auto *countExpr = new (C) ConstructorRefCallExpr(countDotExpr, intTypeExpr);
    auto *assignCallExpr = CallExpr::createImplicit(
        C, dotExpr, {countExpr}, {C.getIdentifier("by")});
    auto *assignExpr = new (C) AssignExpr(addressDeclRefExpr, SourceLoc(), 
                                          assignCallExpr, /*Implicit*/ true);
    
    memberExprs.push_back(callExpr);
    memberExprs.push_back(assignExpr);
  }

  auto *thenBody = BraceStmt::create(C, SourceLoc(), 
                                     C.AllocateCopy(memberExprs),
                                     SourceLoc(), /*implicit*/ true);

  auto *ifStmt = new (C)
      IfStmt(LabeledStmtInfo(), /*IfLoc*/ SourceLoc(), /*Cond*/ cond,
             /*Then*/ thenBody, /*ElseLoc*/ SourceLoc(), /*Else*/ nullptr, 
             /*implicit*/ true);
  
  funcDecl->setBody(BraceStmt::create(C, SourceLoc(), {ifStmt}, SourceLoc(),
                                      /*implicit*/ true));
}

// Synthesize function declaration for a `TensorArrayProtocol` 
// method requirement.
static ValueDecl *deriveTensorArrayProtocol_method(
    DerivedConformance &derived, Identifier methodName, Identifier argumentName,
    Identifier parameterName, Type parameterType, Type returnType,
    AbstractFunctionDecl::BodySynthesizer bodySynthesizer) {
  auto nominal = derived.Nominal;
  auto &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();

  auto *param =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        argumentName, SourceLoc(), parameterName, parentDC);
  param->setInterfaceType(parameterType);
  ParameterList *params = ParameterList::create(C, {param});

  DeclName declName(C, methodName, params);
  auto funcDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                   SourceLoc(), declName, SourceLoc(),
                                   /*Throws*/ false, SourceLoc(),
                                   /*GenericParams*/ nullptr, params,
                                   TypeLoc::withoutLoc(returnType), parentDC);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(bodySynthesizer);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    funcDecl->setGenericEnvironment(env);
  funcDecl->computeType();
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  funcDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({funcDecl});
  C.addSynthesizedDecl(funcDecl);

  return funcDecl;
}

// Synthesize the `_unpackTensorHandles(into:)` function declaration.
static ValueDecl 
*deriveTensorArrayProtocol_unpackTensorHandles(DerivedConformance &derived) {
  auto &C = derived.TC.Context;

  // Obtain the address type.
  auto cTensorHandleType = C.getOpaquePointerDecl()->getDeclaredType();
  Type baseAddressType = BoundGenericType::get(
      C.getUnsafeMutablePointerDecl(), Type(), {cTensorHandleType});
  Type addressType = BoundGenericType::get(
      C.getOptionalDecl(), Type(), {baseAddressType});
  
  Type voidType = C.getVoidDecl()->getDeclaredInterfaceType();

  return deriveTensorArrayProtocol_method(
      derived, C.Id_unpackTensorHandles, C.getIdentifier("into"),
      C.getIdentifier("address"), addressType, voidType,
      deriveBodyTensorArrayProtocol_unpackTensorHandles);
}

ValueDecl *DerivedConformance::deriveTensorArrayProtocol(
    ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_unpackTensorHandles)
    return deriveTensorArrayProtocol_unpackTensorHandles(*this);
  TC.diagnose(requirement->getLoc(), 
              diag::broken_tensor_array_protocol_requirement);
  return nullptr;
}
