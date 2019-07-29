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
                                                 None);
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

// Return the protocol requirement with the specified name.
static ValueDecl *getProtocolRequirement(ProtocolDecl *proto, DeclName name) {
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
static std::pair<BraceStmt *, bool>
deriveBodyTensorArrayProtocol_unpackTensorHandles(
    AbstractFunctionDecl *funcDecl, void *) {
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
      /*IsStatic*/ false, VarDecl::Introducer::Var, /*IsCaptureList*/ false,
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

  // Iterate through the `TensorArrayProtocol`-conforming members and call
  // `member._unpackTensorHandles(into:)`.
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
      memberMethodDecl = confRef->getConcrete()->getWitnessDecl(methodReq);
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

  auto *braceStmt = BraceStmt::create(C, SourceLoc(), {ifStmt}, SourceLoc(),
                                      /*implicit*/ true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
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
      new (C) ParamDecl(ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(),
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
  funcDecl->setBodySynthesizer(bodySynthesizer.Fn, bodySynthesizer.Context);

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
      {deriveBodyTensorArrayProtocol_unpackTensorHandles, nullptr});
}

/// Derive the body for the '_tensorHandleCount' getter.
static std::pair<BraceStmt *, bool>
deriveBodyTensorArrayProtocol_tensorHandleCount(AbstractFunctionDecl *funcDecl,
                                                void *) {
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
  auto *braceStmt = BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
}

/// Derive a `_tensorHandleCount` implementation.
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
    tensorHandleCountDecl, returnType);
  getterDecl->setBodySynthesizer(
    deriveBodyTensorArrayProtocol_tensorHandleCount, nullptr);
  tensorHandleCountDecl->setAccessors(SourceLoc(), {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext(
    {getterDecl, tensorHandleCountDecl, patDecl});

  return tensorHandleCountDecl;
}

/// Derive the body for the '_typeList' getter.
static std::pair<BraceStmt *, bool>
deriveBodyTensorArrayProtocol_typeList(AbstractFunctionDecl *funcDecl, void *) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *tensorGroupProto = C.getProtocol(KnownProtocolKind::TensorGroup);
  auto *typeListReq = getProtocolRequirement(tensorGroupProto, C.Id_typeList);

  // Concatenate all member `_typeList` arrays.
  Type arrayType = BoundGenericType::get(
    C.getArrayDecl(), Type(),
    {C.getTensorDataTypeDecl()->getDeclaredInterfaceType()});
  auto *arrayTypeExpr = TypeExpr::createImplicit(arrayType, C);
  auto plusOpLookup = C.getArrayDecl()->lookupDirect(C.getIdentifier("+"));
  assert(plusOpLookup.size() == 1 && "Ambiguous 'Array.+' operator.");
  ValueDecl *plusOpDecl = plusOpLookup.front();
  auto plusOpDRE = new (C)
      DeclRefExpr(plusOpDecl, DeclNameLoc(), /*Implicit*/ true);
  auto plusOpExpr = new (C)
      DotSyntaxCallExpr(plusOpDRE, SourceLoc(), arrayTypeExpr);
  Expr *typeListExpr = ArrayExpr::create(C, SourceLoc(), {}, {}, SourceLoc());
  for (auto member : nominal->getStoredProperties()) {
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto *memberTypeListExpr = new (C)
        MemberRefExpr(memberTypeExpr, SourceLoc(), typeListReq,
                      DeclNameLoc(), /*Implicit*/ true);
    // Create expression `lhsArg + rhsArg`.
    auto *plusOpArgs =
        TupleExpr::create(C, SourceLoc(), {typeListExpr, memberTypeListExpr},
                          {}, {}, SourceLoc(), /*HasTrailingClosure*/ false,
                          /*Implicit*/ true);
    typeListExpr = new (C) BinaryExpr(plusOpExpr, plusOpArgs,
                                      /*Implicit*/ true);
  }

  // Return the resulting data types array.
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), typeListExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), {returnStmt}, SourceLoc(),
                                 /*Implicit*/ true);
  auto *braceStmt = BraceStmt::create(C, SourceLoc(), {body}, SourceLoc(),
                                      /*Implicit*/ true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
}

/// Derive a `_typeList` implementation.
static ValueDecl *deriveTensorArrayProtocol_typeList(
    DerivedConformance &derived) {
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
      C.Id_typeList, returnType, returnType, /*isStatic*/ false,
      /*isFinal*/ false);

  // Add `@inlinable` to the `_typeList` declaration.
  if (nominal->getEffectiveAccess() > AccessLevel::Internal)
    typeListDecl->getAttrs().add(new (C) InlinableAttr(/*implicit*/ true));

  // Create `_typeList` getter.
  auto *getterDecl = derived.declareDerivedPropertyGetter(
      typeListDecl, returnType);
  getterDecl->setBodySynthesizer(
      deriveBodyTensorArrayProtocol_typeList, nullptr);
  typeListDecl->setAccessors(SourceLoc(), {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext({getterDecl, typeListDecl, patDecl});

  return typeListDecl;
}

// Synthesize body for `init(_owning:count:)`.
static std::pair<BraceStmt *, bool>
deriveBodyTensorArrayProtocol_init(AbstractFunctionDecl *funcDecl, void *) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Obtain the address type.
  auto cTensorHandleType = C.getOpaquePointerDecl()->getDeclaredType();
  auto baseAddressType = BoundGenericType::get(
      C.getUnsafePointerDecl(), Type(), {cTensorHandleType});
  auto addressType = BoundGenericType::get(
      C.getOptionalDecl(), Type(), {baseAddressType});
  auto *addressTE = TypeExpr::createImplicit(addressType, C);

  // Get references to `self` and parameter declarations.
  auto *selfDecl = funcDecl->getImplicitSelfDecl();
  auto *selfDRE = new (C)
      DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);
  auto *paramDecl = funcDecl->getParameters()->get(0);
  auto *paramDRE = new (C)
      DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit*/ true);

  // Create an `if var` statement for the current address.
  VarDecl *currAddressDecl = new (C) VarDecl(
      /*IsStatic*/ false, VarDecl::Introducer::Var, /*IsCaptureList*/ false,
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

  // Get the necessary protocol requirements.
  auto *tensorGroupProto = C.getProtocol(KnownProtocolKind::TensorGroup);
  auto *tensorArrayProto = C.getProtocol(
      KnownProtocolKind::TensorArrayProtocol);
  auto initName = DeclName(
      C, DeclBaseName::createConstructor(), {C.getIdentifier("_owning")});
  auto *initReq = getProtocolRequirement(tensorGroupProto, initName);
  auto *tensorHandleCountReq = getProtocolRequirement(
      tensorArrayProto, C.Id_tensorHandleCount);

  Type intType = C.getIntDecl()->getDeclaredType();
  TypeExpr *intTE = TypeExpr::createImplicit(intType, C);

  // Iterate over members and call `self.member = MemberType(_owning:)`.
  llvm::SmallVector<ASTNode, 2> thenMemberExprs;
  llvm::SmallVector<ASTNode, 2> elseMemberExprs;
  for (auto member : nominal->getStoredProperties()) {
    auto memberType = parentDC->mapTypeIntoContext(
        member->getValueInterfaceType());
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(
        memberType, tensorGroupProto);
    assert(confRef && "Member does not conform to `TensorGroup`");

    // Get member type's constructor, e.g. `MemberType.init(_owning:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberInitDecl = initReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the constructor.
    if (confRef->isConcrete())
      memberInitDecl = confRef->getConcrete()->getWitnessDecl(initReq);
    assert(memberInitDecl && "Member constructor declaration must exist");
    auto memberInitDRE = new (C) DeclRefExpr(
        memberInitDecl, DeclNameLoc(), /*implicit*/ true);
    memberInitDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member constructor: `MemberType.init(_owning:)`.
    auto *memberInitExpr = new (C) ConstructorRefCallExpr(
        memberInitDRE, memberTypeExpr);

    auto *addressDRE = new (C) DeclRefExpr(
        currAddressDecl, DeclNameLoc(), /*implicit*/ true);
    auto *loadExpr = new (C) LoadExpr(addressDRE, baseAddressType);

    // Initialize the member using its `TensorGroup` constructor.
    // Note that, initialization is dependent on the branch of the
    // if-statement taken.
    auto *thenInitExpr = new (C) InjectIntoOptionalExpr(loadExpr, addressType);
    auto *thenInitCallExpr = CallExpr::createImplicit(
        C, memberInitExpr, {thenInitExpr}, {C.getIdentifier("_owning")});

    // Create a nil expression with type `UnsafePointer<CTensorHandle>?` for the
    // `else` branch.
    auto *nilDecl = C.getOptionalNoneDecl();
    auto *nilDRE = new (C) DeclRefExpr(
        nilDecl, DeclNameLoc(), /*implicit*/ true);
    auto *elseInitExpr = new (C) DotSyntaxCallExpr(
        nilDRE, SourceLoc(), addressTE);
    auto *elseInitCallExpr = CallExpr::createImplicit(
        C, memberInitExpr, {elseInitExpr}, {C.getIdentifier("_owning")});

    // Assign the current member to the result of the initializer call.
    auto *memberDRE = new (C) MemberRefExpr(
        selfDRE, SourceLoc(), member, DeclNameLoc(), /*Implicit*/ true);

    auto *thenAssignMemberExpr = new (C) AssignExpr(
        memberDRE, SourceLoc(), thenInitCallExpr, /*Implicit*/ true);
    auto *elseAssignMemberExpr = new (C) AssignExpr(
        memberDRE, SourceLoc(), elseInitCallExpr, /*Implicit*/ true);

    thenMemberExprs.push_back(thenAssignMemberExpr);
    elseMemberExprs.push_back(elseAssignMemberExpr);

    // Advance the current address.
    DeclName advancedName(C, C.getIdentifier("advanced"),
                          {C.getIdentifier("by")});
    auto *advancedMethodExpr =
        new (C) UnresolvedDotExpr(addressDRE, SourceLoc(),
                                  advancedName, DeclNameLoc(),
                                  /*Implicit*/ true);

    // Obtain `MemberType._tensorHandleCount`.
    auto *memberCountMRE = new (C) MemberRefExpr(
        memberDRE, SourceLoc(), tensorHandleCountReq, DeclNameLoc(),
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
    auto *assignAddrCallExpr = CallExpr::createImplicit(
        C, advancedMethodExpr, {intInitCallExpr}, {C.getIdentifier("by")});
    auto *assignAddrExpr = new (C) AssignExpr(addressDRE, SourceLoc(),
                                              assignAddrCallExpr,
                                              /*Implicit*/ true);

    thenMemberExprs.push_back(assignAddrExpr);
  }

  auto *thenBody = BraceStmt::create(
      C, SourceLoc(), C.AllocateCopy(thenMemberExprs), SourceLoc(),
      /*implicit*/ true);

  auto *elseBody = BraceStmt::create(
      C, SourceLoc(), C.AllocateCopy(elseMemberExprs), SourceLoc(),
      /*implicit*/ true);

  auto *ifStmt = new (C)
      IfStmt(LabeledStmtInfo(), /*IfLoc*/ SourceLoc(),
             /*Cond*/ C.AllocateCopy(cond), /*Then*/ thenBody,
             /*ElseLoc*/ SourceLoc(), /*Else*/ elseBody, /*implicit*/ true);

  auto *braceStmt = BraceStmt::create(C, SourceLoc(), {ifStmt}, SourceLoc(),
                                      /*implicit*/ true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
}

// Synthesize the `init(_owning:count:)` function declaration.
static ValueDecl
*deriveTensorArrayProtocol_init(DerivedConformance &derived) {
  auto &C = derived.TC.Context;
  auto nominal = derived.Nominal;
  auto parentDC = derived.getConformanceContext();

  // Obtain the address type.
  auto cTensorHandleType = C.getOpaquePointerDecl()->getDeclaredType();
  Type baseAddressType = BoundGenericType::get(
      C.getUnsafePointerDecl(), Type(), {cTensorHandleType});
  Type addressType = BoundGenericType::get(
      C.getOptionalDecl(), Type(), {baseAddressType});
  Type intType = C.getIntDecl()->getDeclaredType();

  auto *param1 = new (C) ParamDecl(
      ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(),
      C.getIdentifier("_owning"), SourceLoc(), C.getIdentifier("tensorHandles"),
      parentDC);
  param1->setInterfaceType(addressType);
  auto *param2 = new (C) ParamDecl(
      ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(),
      C.getIdentifier("count"), SourceLoc(), C.getIdentifier("count"),
      parentDC);
  param2->setInterfaceType(intType);
  ParameterList *params = ParameterList::create(C, {param1, param2});

  DeclName name(C, DeclBaseName::createConstructor(), params);
  auto *initDecl =
      new (C) ConstructorDecl(name, SourceLoc(), OTK_None, SourceLoc(),
                              /*Throws*/ false, SourceLoc(), params,
                              /*GenericParams*/ nullptr, parentDC);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(deriveBodyTensorArrayProtocol_init, nullptr);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    initDecl->setGenericEnvironment(env);
  initDecl->computeType(AnyFunctionType::ExtInfo().withThrows(false));
  initDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  initDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({initDecl});
  C.addSynthesizedDecl(initDecl);

  return initDecl;
}

ValueDecl *DerivedConformance::deriveTensorArrayProtocol(
    ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_unpackTensorHandles)
    return deriveTensorArrayProtocol_unpackTensorHandles(*this);
  if (requirement->getBaseName() == TC.Context.Id_tensorHandleCount)
    return deriveTensorArrayProtocol_tensorHandleCount(*this);
  if (requirement->getBaseName() == TC.Context.Id_typeList)
    return deriveTensorArrayProtocol_typeList(*this);
  if (requirement->getBaseName() == DeclBaseName::createConstructor())
    return deriveTensorArrayProtocol_init(*this);
  TC.diagnose(requirement->getLoc(),
              diag::broken_tensor_array_protocol_requirement);
  return nullptr;
}
