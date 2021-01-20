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
  assert(false);
}

/// Derive the declaration of Actor's resolve initializer.
///
/// Swift signature:
/// ```
///   init(resolve address: ActorAddress, using transport: ActorTransport) throws
/// ```
static ValueDecl *deriveDistributedActor_init_resolve(DerivedConformance &derived) {
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "TODO IMPLEMENT THIS SYNTHESIS");

  return nullptr;
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
  // distributed actor class Greeter {
  //   // Already derived by this point if possible.
  //   @derived let actorTransport: ActorTransport
  //   @derived let address: ActorAddress
  //
  //   @derived init(transport: ActorTransport) throws { // TODO: make it throwing?
  //     self.actorTransport = transport
  //     // self.address = try transport.allocateAddress(self)// TODO: implement this
  //   }
  // }

  // The enclosing type decl.
  auto conformanceDC = initDecl->getDeclContext();
  auto *targetDecl = conformanceDC->getSelfNominalTypeDecl();

  auto *funcDC = cast<DeclContext>(initDecl);
  auto &C = funcDC->getASTContext();

  SmallVector<ASTNode, 2> statements;

  auto transportParam = initDecl->getParameters()->get(0);
  auto *transportExpr = new (C) DeclRefExpr(ConcreteDeclRef(transportParam),
                                            DeclNameLoc(), /*Implicit=*/true);

  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);

  // `self.actorTransport = transport`
  auto *varTransportExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
                                                             C.Id_actorTransport);
  auto *assignTransportExpr = new (C) AssignExpr(
      varTransportExpr, SourceLoc(), transportExpr, /*Implicit=*/true);
  statements.push_back(assignTransportExpr);

  // `self.actorAddress = transport.assignAddress(Self.self)`
//  auto *assignAddressExpr = new (C) AssignExpr(
//      varTransportExpr, SourceLoc(), transportExpr, /*Implicit=*/true);
//  statements.push_back(assignTransportExpr);

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
      /*implicit=*/true);

  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "INIT transport BODY:");
  initDecl->dump();

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
  // Params: (transport actorTransport: ActorTransport)
  auto transportType = C.getActorTransportDecl()->getDeclaredInterfaceType();
  auto *transportParamDecl = new (C) ParamDecl(
      SourceLoc(), SourceLoc(), C.Id_transport,
      SourceLoc(), C.Id_actorTransport, conformanceDC);
  transportParamDecl->setImplicit();
  transportParamDecl->setSpecifier(ParamSpecifier::Default);
  transportParamDecl->setInterfaceType(transportType);
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "param prepared");

  auto *paramList = ParameterList::createWithoutLoc(transportParamDecl);

  // Func name: init(transport:)
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "init func");
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  auto *initDecl =
      new (C) ConstructorDecl(name, SourceLoc(),
                              /*Failable=*/false, SourceLoc(),
                              /*Throws=*/false, SourceLoc(), paramList, // TODO: make it throws?
                              /*GenericParams=*/nullptr, conformanceDC);
  initDecl->setImplicit();
  initDecl->setSynthesized(); // TODO: consider making throwing
  initDecl->setBodySynthesizer(&deriveBodyDistributedActor_init_transport);
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "init func prepared");

  // This constructor is 'required', all distributed actors MUST invoke it.
  // TODO: this makes sense I guess, and we should ban defining such constructor at all.
  auto *reqAttr = new (C) RequiredAttr(/*IsImplicit*/true);
  initDecl->getAttrs().add(reqAttr);

  initDecl->copyFormalAccessFrom(derived.Nominal,
                                 /*sourceIsParentContext=*/true);
  derived.addMembersToConformanceContext({initDecl});
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "added");

  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "INIT DECL:");
  initDecl->dump();

  return initDecl;

//  fprintf(stderr, "[%s:%d] >> TODO: SYNTHESIZE (%s)  \n", __FILE__, __LINE__, __FUNCTION__);
//  // TODO: synthesize the initializer accepting the transport,
//  // - store the transport as actorTransport
//  // - invoke the transport to allocate an address, store it as actorAddress
//
//  return nullptr;
}

// ==== Properties -------------------------------------------------------------

/// Derive the declaration of Actor's actorTransport.
static ValueDecl *deriveDistributedActor_actorTransport(DerivedConformance &derived) {
  ASTContext &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  auto transportType = C.getActorTransportDecl()->getDeclaredInterfaceType();
  VarDecl *varDecl = new (C) VarDecl(/*IsStatic*/false, VarDecl::Introducer::Let,
                                               SourceLoc(), C.Id_actorTransport, conformanceDC);
  varDecl->setInterfaceType(transportType);
  varDecl->setImplicit();

  derived.addMembersToConformanceContext({varDecl});

  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "VAR DECL (actorTransport):");
  varDecl->dump();

  return varDecl;
}


static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_property_getter_address(AbstractFunctionDecl *varDecl, void *) {
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "TRY actorTransport getter BODY:");

  // The enclosing type decl.
  auto conformanceDC = varDecl->getDeclContext();
  auto *targetDecl = conformanceDC->getSelfNominalTypeDecl();

  auto *funcDC = cast<DeclContext>(varDecl);
  auto &C = funcDC->getASTContext();

  // self.actorTransport
  auto *selfRef = DerivedConformance::createSelfDeclRef(varDecl);
  auto *varExpr = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                        ConcreteDeclRef(varDecl),
                                        DeclNameLoc(), /*Implicit=*/true);

  SmallVector<ASTNode, 1> statements;

  // TODO: return the property from this getter
  statements.push_back(varExpr);

  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "actorTransport getter BODY:");
  varDecl->dump();

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
      /*implicit=*/true);
  return { body, /*isTypeChecked=*/false };
}

/// Derive the declaration of Actor's actorAddress.
static ValueDecl *deriveDistributedActor_actorAddress(DerivedConformance &derived) {
  ASTContext &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  auto type = C.getActorAddressDecl()->getDeclaredInterfaceType();

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) =
      derived.declareDerivedProperty(C.Id_actorAddress, type, type,
                                     /*isStatic=*/false, /*isFinal=*/false);
  // Define the getter.
  auto *getterDecl = derived.addGetterToReadOnlyDerivedProperty(propDecl, type);

  // TODO: doing this causes a crash, do we need this though?
  // propDecl->setImplInfo(StorageImplInfo::getSimpleStored(StorageIsNotMutable));

  // Synthesize the body.
  getterDecl->setBodySynthesizer(&deriveBodyDistributedActor_property_getter_address);

  derived.addMembersToConformanceContext({propDecl, pbDecl});

  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "PROP DECL (actorAddress):");
  propDecl->dump();
  fprintf(stderr, "[%s:%d] >> (%s) %s  \n", __FILE__, __LINE__, __FUNCTION__, "PB DECL (actorAddress):");
  pbDecl->dump();

  return propDecl;
}

// ==== ------------------------------------------------------------------------

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  ASTContext &C = ConformanceDecl->getASTContext();

  const auto name = requirement->getName();
  fprintf(stderr, "[%s:%d] >> (%s) TRY %s \n", __FILE__, __LINE__, __FUNCTION__, name);

  // Synthesize initializers
  if (dyn_cast<ConstructorDecl>(requirement)) {
    const auto name = requirement->getName();
    auto argumentNames = name.getArgumentNames();

    if (argumentNames.size() == 1) {
      // TODO: check param labels too here? but we checked already in DerivedConformances.
      fprintf(stderr, "[%s:%d] >> (%s) init 1 param \n", __FILE__, __LINE__, __FUNCTION__);
      return deriveDistributedActor_init_transport(*this);
    } else if (argumentNames.size() == 2) {
      fprintf(stderr, "[%s:%d] >> (%s) init 2 params \n", __FILE__, __LINE__, __FUNCTION__);
      return deriveDistributedActor_init_resolve(*this);
    }
  }

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
