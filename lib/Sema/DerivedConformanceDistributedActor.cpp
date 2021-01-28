//===--- DerivedConformanceActor.cpp - Derived Actor Conformance ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the Actor protocol.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "TypeCheckConcurrency.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isDistributedActor() && dc == nominal;
}

/// Returns whether the given type is valid for synthesizing the transport
/// initializer.
///
/// Checks to see whether the given type has has already defined such initializer,
/// and if not attempts to synthesize it.
///
/// \param requirement The requirement we want to synthesize.
static bool canSynthesizeInitializer(DerivedConformance &derived, ValueDecl *requirement) {
  return true; // TODO: replace with real impl
}

// ==== Initializers -----------------------------------------------------------

/// Synthesizes the body for
///
/// ```
/// init(resolve address: ActorAddress, using transport: ActorTransport) throws
/// ```
///
/// \param initDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_init_resolve(AbstractFunctionDecl *initDecl, void *) {
  // @derived init(resolve address: ActorAddress, using transport: ActorTransport) throws {
  //   // TODO: implement calling the transport
  //   // switch try transport.resolve(address: address, as: Self.self) {
  //   // case .instance(let instance):
  //   //   self = instance
  //   // case .makeProxy:
  //   // TODO: use RebindSelfInConstructorExpr here?
  //   //   self = <<MAGIC MAKE PROXY>>(address, transport) // TODO: implement this
  //   // }
  // }

  // The enclosing type decl.
  auto conformanceDC = initDecl->getDeclContext();
  auto *targetDecl = conformanceDC->getSelfNominalTypeDecl();

  auto *funcDC = cast<DeclContext>(initDecl);
  auto &C = funcDC->getASTContext();

  SmallVector<ASTNode, 2> statements; // TODO: how many?

  auto addressParam = initDecl->getParameters()->get(0);
  auto *addressExpr = new (C) DeclRefExpr(ConcreteDeclRef(addressParam),
                                            DeclNameLoc(), /*Implicit=*/true);

  auto transportParam = initDecl->getParameters()->get(1);
  auto *transportExpr = new (C) DeclRefExpr(ConcreteDeclRef(transportParam),
                                            DeclNameLoc(), /*Implicit=*/true);

  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);

  // ==== `self.actorTransport = transport`
  auto *varTransportExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
                                                             C.Id_actorTransport);
  auto *assignTransportExpr = new (C) AssignExpr(
      varTransportExpr, SourceLoc(), transportExpr, /*Implicit=*/true);
  statements.push_back(assignTransportExpr);

  // ==== `self.actorAddress = transport.assignAddress<Self>(Self.self)`
  // self.actorAddress
  auto *varAddressExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
                                                           C.Id_actorAddress);
  // TODO implement calling the transport with the address and Self.self
  // FIXME: this must be checking with the transport instead
  auto *assignAddressExpr = new (C) AssignExpr(
      varAddressExpr, SourceLoc(), addressExpr, /*Implicit=*/true);
  statements.push_back(assignAddressExpr);
  // end-of-FIXME: this must be checking with the transport instead

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
      /*implicit=*/true);

//  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "INIT transport BODY:");
//  initDecl->dump();

  return { body, /*isTypeChecked=*/false };}

/// Derive the declaration of Actor's resolve initializer.
///
/// Swift signature:
/// ```
///   init(resolve address: ActorAddress, using transport: ActorTransport) throws
/// ```
static ValueDecl *deriveDistributedActor_init_resolve(DerivedConformance &derived) {
  ASTContext &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  // Expected type: (Self) -> (ActorAddress, ActorTransport) -> (Self)
  //
  // Param: (resolve address: ActorAddress)
  auto addressType = C.getActorAddressDecl()->getDeclaredInterfaceType();
  auto *addressParamDecl = new (C) ParamDecl(
      SourceLoc(), SourceLoc(), C.Id_resolve,
      SourceLoc(), C.Id_address, conformanceDC);
  addressParamDecl->setImplicit();
  addressParamDecl->setSpecifier(ParamSpecifier::Default);
  addressParamDecl->setInterfaceType(addressType);

  // Param: (using transport: ActorTransport)
  auto transportType = C.getActorTransportDecl()->getDeclaredInterfaceType();
  auto *transportParamDecl = new (C) ParamDecl(
      SourceLoc(), SourceLoc(), C.Id_using,
      SourceLoc(), C.Id_transport, conformanceDC);
  transportParamDecl->setImplicit();
  transportParamDecl->setSpecifier(ParamSpecifier::Default);
  transportParamDecl->setInterfaceType(transportType);

  auto *paramList = ParameterList::create(
      C,
      /*LParenLoc=*/SourceLoc(),
      /*params=*/{addressParamDecl, transportParamDecl},
      /*RParenLoc=*/SourceLoc()
  );

  // Func name: init(resolve:using:)
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  auto *initDecl =
      new (C) ConstructorDecl(name, SourceLoc(),
          /*Failable=*/false, SourceLoc(),
          /*Throws=*/false, SourceLoc(), paramList, // TODO: make throws.
          /*GenericParams=*/nullptr, conformanceDC);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(&deriveBodyDistributedActor_init_resolve);

  initDecl->copyFormalAccessFrom(derived.Nominal,
                                 /*sourceIsParentContext=*/true);
  derived.addMembersToConformanceContext({initDecl});
  return initDecl;
}

/// Creates a new \c CallExpr representing
///
///     transport.assignAddress(Self.self)
///
/// \param C The AST context to create the expression in.
///
/// \param DC The \c DeclContext to create any decls in.
///
/// \param base The base expression to make the call on.
///
/// \param returnType The return type of the call.
///
/// \param param The parameter to the call.
static CallExpr *createTransportAssignAddressCall(ASTContext &C,
                                                  DeclContext *DC,
                                                  Expr *base, Type returnType,
                                                  Type param) {
  // (_ actorType:)
  auto *paramDecl = new (C) ParamDecl(SourceLoc(),
                                      SourceLoc(), Identifier(),
                                      SourceLoc(), C.Id_actorType, DC);
  paramDecl->setImplicit();
  paramDecl->setSpecifier(ParamSpecifier::Default);
  paramDecl->setInterfaceType(returnType);

  // transport.assignAddress(_:) expr
  auto *paramList = ParameterList::createWithoutLoc(paramDecl);
  auto *unboundCall = UnresolvedDotExpr::createImplicit(C, base,
                                                        C.Id_assignAddress,
                                                        paramList);

  // DC->mapTypeIntoContext(param->getInterfaceType());
  auto *selfTypeExpr = TypeExpr::createImplicit(param, C);
  auto *dotSelfTypeExpr = new (C) DotSelfExpr(selfTypeExpr, SourceLoc(),
                                              SourceLoc(), param);

  // Full bound self.assignAddress(Self.self) call
  Expr *args[1] = {dotSelfTypeExpr};
  Identifier argLabels[1] = {Identifier()};
  return CallExpr::createImplicit(C, unboundCall, C.AllocateCopy(args),
                                  C.AllocateCopy(argLabels));
}

/// Synthesizes the body for
///
/// ```
/// init(transport: ActorTransport)
/// ```
///
/// \param initDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_init_transport(AbstractFunctionDecl *initDecl, void *) {
  // @derived init(transport: ActorTransport) {
  //   self.actorTransport = transport
  //   self.actorAddress = try transport.assignAddress(Self.self)
  // }

  // The enclosing type decl.
  auto conformanceDC = initDecl->getDeclContext();

  auto *funcDC = cast<DeclContext>(initDecl);
  ASTContext &C = funcDC->getASTContext();

  SmallVector<ASTNode, 2> statements;

  auto transportParam = initDecl->getParameters()->get(0);
  auto *transportExpr = new (C) DeclRefExpr(ConcreteDeclRef(transportParam),
                                            DeclNameLoc(), /*Implicit=*/true);

  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);

  // ==== `self.actorTransport = transport`
  auto *varTransportExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
                                                             C.Id_actorTransport);
  auto *assignTransportExpr = new (C) AssignExpr(
      varTransportExpr, SourceLoc(), transportExpr, /*Implicit=*/true);
  statements.push_back(assignTransportExpr);

  // ==== `self.actorAddress = transport.assignAddress<Self>(Self.self)`
  // self.actorAddress
  auto *varAddressExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
                                                           C.Id_actorAddress);
  // Bound transport.assignAddress(Self.self) call
  auto addressType = C.getActorAddressDecl()->getDeclaredInterfaceType();
  auto selfType = funcDC->getInnermostTypeContext()->getSelfTypeInContext();
  auto *targetDecl = conformanceDC->getSelfNominalTypeDecl(); // TODO: maybe this instead of self type?
  auto *callExpr = createTransportAssignAddressCall(C, funcDC,
                                                    /*base=*/transportExpr,
                                                    /*returnType=*/addressType,
                                                    /*param=*/selfType);
  auto *assignAddressExpr = new (C) AssignExpr(
      varAddressExpr, SourceLoc(), callExpr, /*Implicit=*/true);
  statements.push_back(assignAddressExpr);

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
      /*implicit=*/true);

  return { body, /*isTypeChecked=*/false };
}


/// Derive the declaration of Actor's local initializer.
/// Swift signature:
/// ```
///   init(transport actorTransport: ActorTransport) { ... }
/// ```
static ValueDecl *deriveDistributedActor_init_transport(DerivedConformance &derived) {
  ASTContext &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  // Expected type: (Self) -> (ActorTransport) -> (Self)
  //
  // Params: (transport transport: ActorTransport)
  auto transportType = C.getActorTransportDecl()->getDeclaredInterfaceType();
  auto *transportParamDecl = new (C) ParamDecl(
      SourceLoc(), SourceLoc(), C.Id_transport,
      SourceLoc(), C.Id_transport, conformanceDC);
  transportParamDecl->setImplicit();
  transportParamDecl->setSpecifier(ParamSpecifier::Default);
  transportParamDecl->setInterfaceType(transportType);

  auto *paramList = ParameterList::createWithoutLoc(transportParamDecl);

  // Func name: init(transport:)
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  auto *initDecl =
      new (C) ConstructorDecl(name, SourceLoc(),
                              /*Failable=*/false, SourceLoc(),
                              /*Throws=*/false, SourceLoc(), paramList, // TODO: make it throws?
                              /*GenericParams=*/nullptr, conformanceDC);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(&deriveBodyDistributedActor_init_transport);

//  // This constructor is 'required', all distributed actors MUST invoke it.
//  // TODO: this makes sense I guess, and we should ban defining such constructor at all.
//  auto *reqAttr = new (C) RequiredAttr(/*IsImplicit*/true);
//  initDecl->getAttrs().add(reqAttr);

  initDecl->copyFormalAccessFrom(derived.Nominal,
                                 /*sourceIsParentContext=*/true);

  derived.addMembersToConformanceContext({initDecl});
  return initDecl;
}

// ==== Properties -------------------------------------------------------------

static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_property_getter_actorTransport(AbstractFunctionDecl *funcDecl, void *) {
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "TRY actorTransport getter BODY:");

  auto *parentDC = funcDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  auto *selfRef = DerivedConformance::createSelfDeclRef(funcDecl);
  auto *memberRef =
      UnresolvedDotExpr::createImplicit(C, selfRef, C.Id_actorTransport);

  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), memberRef);
  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                 SourceLoc());
  return { body, /*isTypeChecked=*/false };
}

/// Derive the declaration of Actor's actorTransport.
static ValueDecl *deriveDistributedActor_actorTransport(DerivedConformance &derived) {
  ASTContext &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  auto transportType = C.getActorTransportDecl()->getDeclaredInterfaceType();

  // Defined the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedConstantProperty(
      C.Id_actorTransport, transportType, transportType,
      /*isStatic*/ false, /*isFinal*/ true);

  derived.addMembersToConformanceContext({propDecl, pbDecl});
//  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "VAR DECL (actorTransport):");
//  propDecl->dump();
//  pbDecl->dump();
//  getterDecl->dump();
  return propDecl;
}

static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_property_getter_actorAddress(AbstractFunctionDecl *funcDecl, void *) {
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "TRY actorAddress getter BODY:");

  auto *parentDC = funcDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  auto *selfRef = DerivedConformance::createSelfDeclRef(funcDecl);
  auto *memberRef =
      UnresolvedDotExpr::createImplicit(C, selfRef, C.Id_actorAddress);

  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), memberRef);
  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                 SourceLoc());
  return { body, /*isTypeChecked=*/false };
}

/// Derive the declaration of Actor's actorAddress.
static ValueDecl *deriveDistributedActor_actorAddress(DerivedConformance &derived) {
  ASTContext &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  auto addressType = C.getActorAddressDecl()->getDeclaredInterfaceType();

  // Defined the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedConstantProperty(
      C.Id_actorAddress, addressType, addressType,
      /*isStatic*/ false, /*isFinal*/ true);

  // Define the getter.
//  auto *getterDecl =
//      derived.addGetterToReadOnlyDerivedProperty(propDecl, addressType);
//  getterDecl->setBodySynthesizer(deriveBodyDistributedActor_property_getter_actorAddress, nullptr);

  derived.addMembersToConformanceContext({propDecl, pbDecl});
  return propDecl;
}

// ==== ------------------------------------------------------------------------

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  ASTContext &C = ConformanceDecl->getASTContext();

  const auto name = requirement->getName();
  fprintf(stderr, "[%s:%d] >> (%s) TRY %s \n", __FILE__, __LINE__, __FUNCTION__, name);

  // Synthesize initializers // TODO: this is actually now done earlier, no need to do here?
//  if (dyn_cast<ConstructorDecl>(requirement)) {
//    const auto name = requirement->getName();
//    auto argumentNames = name.getArgumentNames();
//
//    // TODO: check param labels too here? but we checked already in DerivedConformances.
//    if (argumentNames.size() == 1 &&
//        argumentNames[0] == C.Id_transport) {
//      fprintf(stderr, "[%s:%d] >> (%s) init 1 param \n", __FILE__, __LINE__, __FUNCTION__);
//      return deriveDistributedActor_init_transport(*this);
//    } else if (argumentNames.size() == 2 &&
//               argumentNames[0] == C.Id_resolve &&
//               argumentNames[1] == C.Id_using) {
//      fprintf(stderr, "[%s:%d] >> (%s) init 2 params \n", __FILE__, __LINE__, __FUNCTION__);
//      return deriveDistributedActor_init_resolve(*this);
//    }
//  }

  // Synthesize properties
  if (isa<VarDecl>(requirement)) {
    if (VarDecl::isDistributedActorTransportName(Context, name)) {
      fprintf(stderr, "[%s:%d] >> (%s) %s \n", __FILE__, __LINE__, __FUNCTION__, "actorTransport");
      return deriveDistributedActor_actorTransport(*this);
    }

    if (VarDecl::isDistributedActorAddressName(Context, name)) {
      fprintf(stderr, "[%s:%d] >> (%s) %s \n", __FILE__, __LINE__, __FUNCTION__, "actorAddress");
      return deriveDistributedActor_actorAddress(*this);
    }
  }

  // Synthesize functions
  auto func = dyn_cast<FuncDecl>(requirement);
  if (func) {
    fprintf(stderr, "[%s:%d] >> (%s) function .... \n", __FILE__, __LINE__, __FUNCTION__);
    // TODO: derive encode impl
    return nullptr;
  }

 return nullptr;
}
