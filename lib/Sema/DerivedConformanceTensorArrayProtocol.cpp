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

  // Get references to `self` and parameter declarations.
  auto *selfDecl = funcDecl->getImplicitSelfDecl();
  auto *selfDRE = new (C) 
      DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);
  auto *paramDecl = funcDecl->getParameters()->get(0);
  auto *paramDRE = new (C) 
      DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit*/ true);

  // Create an `if var` statement for the current address.
  VarDecl *currAddressDecl = new (C) VarDecl(
      /*IsStatic*/ false, VarDecl::Specifier::Var, /*IsCaptureList*/ false, 
      SourceLoc(), C.getIdentifier("currentAddress"), funcDecl);
  currAddressDecl->setImplicit();
  currAddressDecl->setHasNonPatternBindingInit(true);
  currAddressDecl->setInterfaceType(baseAddressType);
  currAddressDecl->setValidationToChecked();

  Pattern *currAddressPat = new (C)
      NamedPattern(currAddressDecl, /*implicit*/ true);
  currAddressPat = new (C) 
      VarPattern(SourceLoc(), /*isLet*/ false, currAddressPat, 
                 /*implicit*/ true);
  currAddressPat = new (C)
      OptionalSomePattern(currAddressPat, currAddressPat->getEndLoc(), 
                          /*implicit*/ true);
  StmtConditionElement cond[] = {
      StmtConditionElement(SourceLoc(), currAddressPat, /*Init*/ paramDRE)};

  // Get method protocol requirement.
  auto *tensorArrayProto = C.getProtocol(
      KnownProtocolKind::TensorArrayProtocol);
  auto *methodReq = getProtocolRequirement(
      tensorArrayProto, C.Id_unpackTensorHandles);
  auto *countReq = getProtocolRequirement(
      tensorArrayProto, C.Id_tensorHandleCount);

  Type intType = C.getIntDecl()->getDeclaredType();
  TypeExpr *intTE = TypeExpr::createImplicit(intType, C);

  // Go through the member TensorArrayProtocols and call 
  // `_unpackTensorHandles(into:)`.
  llvm::SmallVector<ASTNode, 2> memberExprs;
  for (auto member : nominal->getStoredProperties()) {
    auto memberType = parentDC->mapTypeIntoContext(
        member->getValueInterfaceType());
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(memberType, tensorArrayProto);
    assert(confRef && "Member does not conform to `TensorArrayProtocol`");

    // Get member type's method, e.g. `Member._unpackTensorHandles(into:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberMethodDecl = methodReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      memberMethodDecl = confRef->getConcrete()->getWitnessDecl(
          methodReq, C.getLazyResolver());
    assert(memberMethodDecl && "Member method declaration must exist");
    auto memberMethodDRE = new (C) DeclRefExpr(
        memberMethodDecl, DeclNameLoc(), /*Implicit*/ true);
    memberMethodDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member method: `Member._unpackTensorHandles(into:)`.
    auto *memberDRE = new (C) MemberRefExpr(
        selfDRE, SourceLoc(), member, DeclNameLoc(), /*Implicit*/ true);
    auto memberMethodExpr = new (C) 
        DotSyntaxCallExpr(memberMethodDRE, SourceLoc(), memberDRE);

    // Obtain the method call argument.
    auto *addressDRE = new (C) DeclRefExpr(
      currAddressDecl, DeclNameLoc(), /*implicit*/ true);
    auto *loadExpr = new (C) LoadExpr(addressDRE, baseAddressType);
    auto *injectExpr = new (C) InjectIntoOptionalExpr(loadExpr, addressType);

    auto *callExpr = CallExpr::createImplicit(
        C, memberMethodExpr, {injectExpr}, {C.getIdentifier("into")});
    
    // Advance the current address.
    DeclName advancedName(C, C.getIdentifier("advanced"), 
                          {C.getIdentifier("by")});
    auto *advancedMethodExpr = 
        new (C) UnresolvedDotExpr(addressDRE, SourceLoc(),
                                  advancedName, DeclNameLoc(), 
                                  /*Implicit*/ true);

    // Obtain `Member._tensorHandleCount`.
    auto *memberCountMRE = new (C) MemberRefExpr(
        memberDRE, SourceLoc(), countReq, DeclNameLoc(), 
        /*Implicit*/ true);
    
    // Cast the tensor handle count to Int.
    auto intInitName = DeclName(C, DeclBaseName::createConstructor(), 
                                {Identifier()});
    auto *intInitExpr = 
        new (C) UnresolvedDotExpr(intTE, SourceLoc(), intInitName, 
                                  DeclNameLoc(), /*Implicit*/ true);
    auto *intInitCallExpr = CallExpr::createImplicit(
        C, intInitExpr, {memberCountMRE}, {Identifier()});
    
    // Assign the new address.
    auto *assignCallExpr = CallExpr::createImplicit(
        C, advancedMethodExpr, {intInitCallExpr}, {C.getIdentifier("by")});
    auto *assignExpr = new (C) AssignExpr(addressDRE, SourceLoc(), 
                                          assignCallExpr, /*Implicit*/ true);
    
    memberExprs.push_back(callExpr);
    memberExprs.push_back(assignExpr);
  }

  auto *thenBody = BraceStmt::create(C, SourceLoc(), 
                                     C.AllocateCopy(memberExprs),
                                     SourceLoc(), /*implicit*/ true);

  auto *ifStmt = new (C)
      IfStmt(LabeledStmtInfo(), /*IfLoc*/ SourceLoc(), 
             /*Cond*/ C.AllocateCopy(cond), /*Then*/ thenBody, 
             /*ElseLoc*/ SourceLoc(), /*Else*/ nullptr, /*implicit*/ true);
  
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

/// Derive the body for the '_tensorHandleCount' getter.
static void
deriveBodyTensorArrayProtocol_tensorHandleCount(
    AbstractFunctionDecl *funcDecl) {
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Get references to `self`.
  auto *selfDecl = funcDecl->getImplicitSelfDecl();
  auto *selfDRE = new (C) 
    DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  // Get protocol requirement.
  auto *tensorArrayProto = C.getProtocol(
    KnownProtocolKind::TensorArrayProtocol);
  auto *countReq = getProtocolRequirement(
    tensorArrayProto, C.Id_tensorHandleCount);

  // Concatenate all member `_tensorHandleCount`s.
  Type intType = C.getInt32Decl()->getDeclaredType();
  TypeExpr *intTE = TypeExpr::createImplicit(intType, C);
  auto plusOpLookup = C.getInt32Decl()->lookupDirect(C.getIdentifier("+"));
  assert(plusOpLookup.size() == 1 && "Ambiguous 'Int32.+' operator.");
  ValueDecl *plusOpDecl = plusOpLookup.front();
  auto plusOpDRE = new (C) 
      DeclRefExpr(plusOpDecl, DeclNameLoc(), /*Implicit*/ true);
  auto plusOpExpr = new (C) DotSyntaxCallExpr(plusOpDRE, SourceLoc(), intTE);
  Expr *tensorHandleCountExpr = new (C) 
      IntegerLiteralExpr("0", SourceLoc(), /*implicit*/ true);
  for (auto member : nominal->getStoredProperties()) {
    auto *memberDRE = new (C) MemberRefExpr(
      selfDRE, SourceLoc(), member, DeclNameLoc(), /*Implicit*/ true);
    auto *memberTensorHandleCountExpr = new (C) 
      MemberRefExpr(memberDRE, SourceLoc(), countReq, 
                    DeclNameLoc(), /*Implicit*/ true);
    // Create expression `lhsArg + rhsArg`.
    auto *plusOpArgs = 
        TupleExpr::create(C, SourceLoc(), 
                          {tensorHandleCountExpr, memberTensorHandleCountExpr}, 
                          {}, {}, SourceLoc(), /*HasTrailingClosure*/ false,
                          /*Implicit*/ true);
    tensorHandleCountExpr = new (C) BinaryExpr(plusOpExpr, plusOpArgs, 
                                               /*Implicit*/ true);
  }

  // Return the resulting data types array.
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), tensorHandleCountExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                 /*Implicit*/ true);
  funcDecl->setBody(BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true));
}

/// Derive a '_tensorHandleCount' implementation.
static ValueDecl *deriveTensorArrayProtocol_tensorHandleCount(
    DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  ASTContext &C = TC.Context;

  auto parentDC = derived.getConformanceContext();
  Type intType = C.getInt32Decl()->getDeclaredType();
  auto returnType = parentDC->mapTypeIntoContext(intType);

  // Create `_tensorHandleCount` property declaration.
  VarDecl *tensorHandleCountDecl;
  PatternBindingDecl *patDecl;
  std::tie(tensorHandleCountDecl, patDecl) = derived.declareDerivedProperty(
    C.Id_tensorHandleCount, returnType, returnType, /*isStatic*/ false,
    /*isFinal*/ false);

  // Add `@inlinable` to the `_tensorHandleCount` declaration.
  if (nominal->getEffectiveAccess() > AccessLevel::Internal)
    tensorHandleCountDecl->getAttrs().add(
      new (C) InlinableAttr(/*implicit*/ true));

  // Create `_tensorHandleCount` getter.
  auto *getterDecl = derived.declareDerivedPropertyGetter(
    TC, tensorHandleCountDecl, returnType);
  getterDecl->setBodySynthesizer(
    deriveBodyTensorArrayProtocol_tensorHandleCount);
  tensorHandleCountDecl->setAccessors(StorageImplInfo::getImmutableComputed(),
                                      SourceLoc(), {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext(
    {getterDecl, tensorHandleCountDecl, patDecl});

  return tensorHandleCountDecl;
}

ValueDecl *DerivedConformance::deriveTensorArrayProtocol(
    ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_unpackTensorHandles)
    return deriveTensorArrayProtocol_unpackTensorHandles(*this);
  if (requirement->getBaseName() == TC.Context.Id_tensorHandleCount)
    return deriveTensorArrayProtocol_tensorHandleCount(*this);
  TC.diagnose(requirement->getLoc(), 
              diag::broken_tensor_array_protocol_requirement);
  return nullptr;
}
