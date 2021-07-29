//===--- CodeSynthesis.cpp - Type Checking for Declarations ---------------===//
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

#include "TypeCheckDistributed.h"

#include "CodeSynthesis.h"

#include "TypeChecker.h"
#include "TypeCheckDecl.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "swift/Strings.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Availability.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "DerivedConformances.h"
using namespace swift;

/******************************************************************************/
/******************************* INITIALIZERS *********************************/
/******************************************************************************/

// ==== Distributed Actor: Local Initializer -----------------------------------

/// Creates a new \c CallExpr representing
///
///     transport.assignIdentity(Self.self)
///
/// \param C The AST context to create the expression in.
/// \param DC The \c DeclContext to create any decls in.
/// \param base The base expression to make the call on.
/// \param returnType The return type of the call.
/// \param param The parameter to the call.
static CallExpr *
createCall_DistributedActor_transport_assignIdentity(ASTContext &C,
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

  // transport.assignIdentity(_:) expr
  auto *paramList = ParameterList::createWithoutLoc(paramDecl);
  auto *unboundCall = UnresolvedDotExpr::createImplicit(C, base,
                                                        C.Id_assignIdentity,
                                                        paramList);

  // DC->mapTypeIntoContext(param->getInterfaceType());
  auto *selfTypeExpr = TypeExpr::createImplicit(param, C);
  auto *dotSelfTypeExpr = new (C) DotSelfExpr(selfTypeExpr, SourceLoc(),
                                              SourceLoc(), param);

  // Full bound self.assignIdentity(Self.self) call
  Expr *args[1] = {dotSelfTypeExpr};
  Identifier argLabels[1] = {Identifier()};
  return CallExpr::createImplicit(C, unboundCall, C.AllocateCopy(args),
                                  C.AllocateCopy(argLabels));
}


/// Creates a new \c CallExpr representing
///
///     transport.actorReady(self)
static CallExpr *
createCall_DistributedActor_transport_actorReady(ASTContext &C,
                                                 DeclContext *DC,
                                                 Expr *base, Type actorType,
                                                 Expr *initalizedSelf) {
  // (_ actor:)
  auto *paramDecl = new (C) ParamDecl(SourceLoc(),
                                      SourceLoc(), Identifier(),
                                      SourceLoc(), C.Id_actor, DC);
  paramDecl->setImplicit();
  paramDecl->setSpecifier(ParamSpecifier::Default);
  paramDecl->setInterfaceType(actorType);

  // transport.actorReady(_:) expr
  auto *paramList = ParameterList::createWithoutLoc(paramDecl);
  auto *unboundCall = UnresolvedDotExpr::createImplicit(C, base,
                                                        C.Id_actorReady,
                                                        paramList);

  // Full bound transport.actorReady(self) call
  Expr *args[1] = {initalizedSelf};
  Identifier argLabels[1] = {Identifier()};
  return CallExpr::createImplicit(C, unboundCall, C.AllocateCopy(args),
                                  C.AllocateCopy(argLabels));
}

/// Synthesizes the body of the `init(transport:)` initializer as:
///
/// ```
/// init(transport: ActorTransport)
///   self.actorTransport = transport
///   self.id = try transport.assignIdentity(Self.self)
/// }
/// ```
///
/// \param initDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
createBody_DistributedActor_init_transport(AbstractFunctionDecl *initDecl, void *) {

  auto *funcDC = cast<DeclContext>(initDecl);
  ASTContext &C = funcDC->getASTContext();

  SmallVector<ASTNode, 2> statements;

  auto transportParam = initDecl->getParameters()->get(0);
  auto *transportExpr = new (C) DeclRefExpr(ConcreteDeclRef(transportParam),
                                            DeclNameLoc(), /*Implicit=*/true);

  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);

//  // ==== `self.actorTransport = transport`
//  {
//    // self.actorTransport
//    auto *varTransportExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
//                                                               C.Id_actorTransport);
//    auto *assignTransportExpr = new (C) AssignExpr(
//        varTransportExpr, SourceLoc(), transportExpr, /*Implicit=*/true);
//    statements.push_back(assignTransportExpr);
//  }
//
//  auto selfType = funcDC->getInnermostTypeContext()->getSelfTypeInContext();
//
//  // ==== `self.id = transport.assignIdentity<Self>(Self.self)`
//  {
//    // self.id
//    auto *varIdExpr = UnresolvedDotExpr::createImplicit(C, selfRef, C.Id_id);
//    // Bound transport.assignIdentity(Self.self) call
//    auto anyIdentityType = C.getAnyActorIdentityDecl()->getDeclaredInterfaceType();
//    auto *callExpr = createCall_DistributedActor_transport_assignIdentity(
//        C, funcDC,
//        /*base=*/transportExpr,
//        /*returnType=*/anyIdentityType, // TODO(distributed): move the function to return just ActorIdentity
//        /*param=*/selfType);
//    auto *assignIdExpr = new (C) AssignExpr(
//        varIdExpr, SourceLoc(), callExpr, /*Implicit=*/true);
//    statements.push_back(assignIdExpr);
//  }

  // Initialize the identity and transport stored in the implicitly created storage
  {
    // 1. Obtain the identity assigned by the transport.
    auto selfType = funcDC->getInnermostTypeContext()->getSelfTypeInContext();
    auto anyIdentityType = C.getAnyActorIdentityDecl()->getDeclaredInterfaceType(); // TODO: use the esistential here
    auto identityType = C.getProtocol(KnownProtocolKind::ActorIdentity)->getDeclaredInterfaceType();
    auto *assignIdentityCallExpr = createCall_DistributedActor_transport_assignIdentity(
        C, funcDC,
        /*base=*/transportExpr,
        /*returnType=*/anyIdentityType, // TODO(distributed): move the function to return just ActorIdentity
        /*param=*/selfType);

    auto *typeInfo = DerivedConformance::createSelfDeclRef(initDecl);

    // 2. Obtain built-in function reference.
    auto builtinID = C.getIdentifier("initializeDistributedLocalActor");
    auto *builtinDecl = getBuiltinValueDecl(C, builtinID);
    auto *builtinExpr = new (C) DeclRefExpr(ConcreteDeclRef(builtinDecl),
                                            DeclNameLoc(), /*implicit=*/true);

    statements.push_back(assignIdentityCallExpr);

    // 3. Create the call
////    SmallVector<Expr*, 3> args = {assignIdentityCallExpr, transportExpr, typeInfo};
//    SmallVector<Expr*, 3> args = {selfRef, assignIdentityCallExpr, transportExpr};
//    SmallVector<Identifier, 3> argLabels =
//        {Identifier(), Identifier(), Identifier()};
//
//    auto *call = CallExpr::createImplicit(C, builtinExpr, args, argLabels);
//    statements.push_back(call);
  }

  // FIXME: this must be called once we're done initializing... at end of user inits
  // ---=== Done initializing ===---
  // === `transport.actorReady(self)`
  {
    // Bound transport.actorReady(self) call
    auto selfType = funcDC->getInnermostTypeContext()->getSelfTypeInContext();
    auto *callExpr = createCall_DistributedActor_transport_actorReady(
        C, funcDC,
        /*base=*/transportExpr,
        /*actorType=*/selfType,
        /*param=*/selfRef);
    statements.push_back(callExpr);
  }

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                 /*implicit=*/true);
  return { body, /*isTypeChecked=*/false };
}

/// Synthesizes the
///
/// ```
/// init(transport: ActorTransport)
/// ```
///
/// local initializer.
static ConstructorDecl *
createDistributedActor_init_local(ClassDecl *classDecl,
                                  ASTContext &ctx) {
  auto &C = ctx;
  auto conformanceDC = classDecl;

  // Expected type: (Self) -> (ActorTransport) -> (Self)
  //
  // Params: (transport: ActorTransport)
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
                              /*Async=*/false, SourceLoc(),
                              /*Throws=*/false, SourceLoc(),
                              paramList,
                              /*GenericParams=*/nullptr, conformanceDC);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(&createBody_DistributedActor_init_transport);

  auto *nonIsoAttr = new (C) NonisolatedAttr(/*IsImplicit*/true);
  initDecl->getAttrs().add(nonIsoAttr);

  initDecl->copyFormalAccessFrom(classDecl, /*sourceIsParentContext=*/true);

  return initDecl;
}

// ==== Distributed Actor: Resolve Initializer ---------------------------------
// TODO(distributed): remove resolve initializer in favor of resolve static function

/// Synthesizes the body for
///
/// ```
/// init(resolve address: ActorAddress, using transport: ActorTransport) throws {
///   // TODO(distributed): implement calling the transport
///   switch try transport.resolve(address: address, as: Self.self) {
///   case .instance(let instance):
///     self = instance
///   case .makeProxy:
///   // TODO(distributed): use RebindSelfInConstructorExpr here?
///     self = <<MAGIC MAKE PROXY>>(address, transport) // TODO(distributed): implement this
///   }
/// }
/// ```
///
/// \param initDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
createDistributedActor_init_resolve_body(AbstractFunctionDecl *initDecl, void *) {
  auto *funcDC = cast<DeclContext>(initDecl);
  auto &C = funcDC->getASTContext();

  SmallVector<ASTNode, 2> statements;

  auto idParam = initDecl->getParameters()->get(0);
  auto *idExpr = new (C) DeclRefExpr(ConcreteDeclRef(idParam),
                                     DeclNameLoc(), /*Implicit=*/true);

  auto transportParam = initDecl->getParameters()->get(1);
  auto *transportExpr = new (C) DeclRefExpr(ConcreteDeclRef(transportParam),
                                            DeclNameLoc(), /*Implicit=*/true);

  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);

  // ==== `self.actorTransport = transport`
//  auto *varTransportExpr = UnresolvedDotExpr::createImplicit(
//      C, selfRef, C.Id_actorTransport);
//  auto *assignTransportExpr = new (C) AssignExpr(
//      varTransportExpr, SourceLoc(), transportExpr, /*Implicit=*/true);
//  statements.push_back(assignTransportExpr);

  // ==== `self.id = transport.assignIdentity<Self>(Self.self)`
  // self.id
//  auto *varIdExpr = UnresolvedDotExpr::createImplicit(C, selfRef, C.Id_id);
//  // TODO(distributed): implement calling the transport with the address and Self.self
//  // FIXME(distributed): this must be checking with the transport instead
//  auto *assignIdExpr = new (C) AssignExpr(
//      varIdExpr, SourceLoc(), idExpr, /*Implicit=*/true);
//  statements.push_back(assignIdExpr);
  // end-of-FIXME: this must be checking with the transport instead

  BraceStmt *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                      /*implicit=*/true);

  return { body, /*isTypeChecked=*/false };
}

/// Synthesizes the
///
/// ```
/// init(resolve address: ActorAddress, using transport: ActorTransport) throws
/// ```
///
/// resolve initializer.
// TODO(distributed): will be replaced with resolve function.
static ConstructorDecl *
createDistributedActor_init_resolve(ClassDecl *classDecl,
                                    ASTContext &ctx) {
  auto &C = ctx;
  auto conformanceDC = classDecl;

  // Expected type: (Self) -> (ActorAddress, ActorTransport) -> (Self)
  //
  // Param: (resolve address: AnyActorAddress)
  auto addressType = C.getAnyActorIdentityDecl()->getDeclaredInterfaceType();
  auto *idParamDecl = new (C) ParamDecl(
      SourceLoc(), SourceLoc(), C.Id_resolve,
      SourceLoc(), C.Id_id, conformanceDC);
  idParamDecl->setImplicit();
  idParamDecl->setSpecifier(ParamSpecifier::Default);
  idParamDecl->setInterfaceType(addressType);

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
      /*params=*/{idParamDecl, transportParamDecl},
      /*RParenLoc=*/SourceLoc()
      );

  // Func name: init(resolve:using:)
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  auto *initDecl =
      new (C) ConstructorDecl(name, SourceLoc(),
                              /*Failable=*/false, SourceLoc(),
                              /*Async=*/false, SourceLoc(),
                              /*Throws=*/true, SourceLoc(),
                              paramList,
                              /*GenericParams=*/nullptr, conformanceDC);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(&createDistributedActor_init_resolve_body);

  auto *nonIsoAttr = new (C) NonisolatedAttr(/*IsImplicit*/true);
  initDecl->getAttrs().add(nonIsoAttr);

  initDecl->copyFormalAccessFrom(classDecl, /*sourceIsParentContext=*/true);

  return initDecl;
}

/// Detects which initializer to create, and does so.
static ConstructorDecl *
createDistributedActorInit(ClassDecl *classDecl,
                           ConstructorDecl *requirement,
                           ASTContext &ctx) {
  assert(classDecl->isDistributedActor());

  const auto name = requirement->getName();
  auto argumentNames = name.getArgumentNames();

  switch (argumentNames.size()) {
  case 1: {
    if (requirement->isDistributedActorLocalInit()) {
      return createDistributedActor_init_local(classDecl, ctx);
    }

    break;
  }
//  case 2: {
//    if (requirement->isDistributedActorResolveInit()) {
//      return createDistributedActor_init_resolve(classDecl, ctx);
//    }
//
//    break;
//  }
  }

  return nullptr;
}

static void collectNonOveriddenDistributedActorInits(
    ASTContext& Context,
    ClassDecl *actorDecl,
    SmallVectorImpl<ConstructorDecl *> &results) {
  assert(actorDecl->isDistributedActor());
  auto protoDecl = Context.getProtocol(KnownProtocolKind::DistributedActor);

  //  // Record all of the initializers the actorDecl has implemented.
  //  llvm::SmallPtrSet<ConstructorDecl *, 4> overriddenInits;
  //  for (auto member : actorDecl->getMembers())
  //    if (auto ctor = dyn_cast<ConstructorDecl>(member))
  //      if (!ctor->hasStubImplementation())
  //         // if (auto overridden = ctor->getOverriddenDecl())
  //          overriddenInits.insert(ctor);
  //
  //  actorDecl->synthesizeSemanticMembersIfNeeded(
  //    DeclBaseName::createConstructor());

  NLOptions subOptions = (NL_QualifiedDefault | NL_IgnoreAccessControl);
  SmallVector<ValueDecl *, 4> lookupResults;
  actorDecl->lookupQualified(
      protoDecl, DeclNameRef::createConstructor(),
      subOptions, lookupResults);

  for (auto decl : lookupResults) {
    // Distributed Actor Constructor
    auto daCtor = cast<ConstructorDecl>(decl);

    // TODO(distributed): Don't require it if overriden
    //    if (!overriddenInits.count(daCtor))
    results.push_back(daCtor);
  }
}


/// For a distributed actor, automatically define initializers
/// that match the DistributedActor requirements.
static void addImplicitDistributedActorConstructors(ClassDecl *decl) {
  // Bail out if not a distributed actor definition.
  if (!decl->isDistributedActor())
    return;

  for (auto member : decl->getMembers()) {
    if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
      if (ctor->isRecursiveValidation())
        return;
    }
  }

  decl->setAddedImplicitInitializers();

  // Check whether the user has defined a designated initializer for this class,
  // and whether all of its stored properties have initial values.
  auto &ctx = decl->getASTContext();

  SmallVector<ConstructorDecl *, 4> nonOverridenCtors;
  collectNonOveriddenDistributedActorInits(
      ctx, decl, nonOverridenCtors);

  for (auto *daCtor : nonOverridenCtors) {
    if (auto ctor = createDistributedActorInit(decl, daCtor, ctx)) {
      decl->addMember(ctor);
    }
  }
}

/******************************************************************************/
/******************************** DEINIT **************************************/
/******************************************************************************/

/// A distributed actor's deinit MUST call `transport.resignIdentity` before it
/// is deallocated.
static void addImplicitResignIdentity(ClassDecl *decl) {
  auto &C = decl->getASTContext();

  DestructorDecl *existingDeinit = decl->getDestructor();
  assert(existingDeinit);

  DestructorDecl *deinitDecl = existingDeinit ? existingDeinit :
      new (C) DestructorDecl(SourceLoc(), decl);
//  DestructorDecl *deinitDecl = new (C) DestructorDecl(SourceLoc(), decl);

  BraceStmt *body = deinitDecl->getBody();

  // == Copy all existing statements to the new deinit
  SmallVector<ASTNode, 2> statements; // TODO(distributed): how to init at body statements count size?
  for (auto stmt : body->getElements())
    statements.push_back(stmt); // TODO(distributed): copy?

  // TODO(distributed): INJECT THIS AS FIRST THING IN A DEFER {}

  // == Inject the lifecycle 'resignIdentity' interaction
  // ==== self
  auto *selfRef = DerivedConformance::createSelfDeclRef(deinitDecl);

  // ==== `self.actorTransport`
  auto *varTransportExpr = UnresolvedDotExpr::createImplicit(C, selfRef,
                                                           C.Id_actorTransport);

  // ==== `self.id`
  auto *varIdExpr = UnresolvedDotExpr::createImplicit(C, selfRef, C.Id_id);

  // ==== `self.transport.resignIdentity(self.actorAddress)`
  //  auto resignIdentityRef = new (C) DeclRefExpr(varTransportExpr,
  //  DeclNameLoc(), /*implicit=*/true);
  //  resignIdentityRef->setThrows(false);
  auto resignFuncDecls =
      C.getActorTransportDecl()->lookupDirect(C.Id_resignIdentity);
  assert(resignFuncDecls.size() == 1);
  AbstractFunctionDecl *resignFuncDecl =
      dyn_cast<AbstractFunctionDecl>(resignFuncDecls.front());
  auto resignFuncRef = new (C) DeclRefExpr(resignFuncDecl, DeclNameLoc(),
                                           /*implicit=*/true);

  auto *idParam = new (C) ParamDecl(
      SourceLoc(), SourceLoc(), Identifier(),
      SourceLoc(), C.Id_id, decl);
  idParam->setInterfaceType(C.getActorIdentityDecl()->getInterfaceType());
  idParam->setSpecifier(ParamSpecifier::Default);
  idParam->setImplicit();
  auto *paramList = ParameterList::createWithoutLoc(idParam);

  auto *resignFuncRefRef = UnresolvedDotExpr::createImplicit(
      C, varTransportExpr, C.Id_resignIdentity, paramList);

  Expr *resignIdentityCall = CallExpr::createImplicit(C, resignFuncRefRef,
                                                     { varIdExpr },
                                                     { Identifier() });
  statements.push_back(resignIdentityCall);

  BraceStmt *newBody = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                         /*implicit=*/true);

  deinitDecl->setBody(newBody, AbstractFunctionDecl::BodyKind::TypeChecked); // FIXME(distributed): no idea if Parsed is right, we are NOT type checked I guess?
}

/******************************************************************************/
/******************************** PROPERTIES **********************************/
/******************************************************************************/

// TODO(distributed): deduplicate with 'declareDerivedProperty' from DerivedConformance...
// TODO(distributed): those are actually computed, see addGetterToReadOnlyDerivedProperty
std::pair<VarDecl *, PatternBindingDecl *>
createStoredProperty(ClassDecl *classDecl, ASTContext &ctx,
                     VarDecl::Introducer introducer, Identifier name,
                     Type propertyInterfaceType, Type propertyContextType,
                     bool isStatic, bool isFinal) {
  auto parentDC = classDecl;

  VarDecl *propDecl = new (ctx)
      VarDecl(/*IsStatic*/ isStatic, introducer,
                           SourceLoc(), name, parentDC);
  propDecl->setImplicit();
  propDecl->setSynthesized();
  propDecl->copyFormalAccessFrom(classDecl, /*sourceIsParentContext*/ true);
  propDecl->setInterfaceType(propertyInterfaceType);

  Pattern *propPat = NamedPattern::createImplicit(ctx, propDecl);
  propPat->setType(propertyContextType);

  propPat = TypedPattern::createImplicit(ctx, propPat, propertyContextType);
  propPat->setType(propertyContextType);

  auto *pbDecl = PatternBindingDecl::createImplicit(
      ctx, StaticSpellingKind::None, propPat, /*InitExpr*/ nullptr,
      parentDC);
  return {propDecl, pbDecl};
}

static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_id(AbstractFunctionDecl *getter, void *) {
  // var id: AnyActorIdentity {
  //   get {
  //     return Builtin.buildDistributedActorIdentity(self)
  //   }
  // }
  ASTContext &ctx = getter->getASTContext();

  // Build a reference to self.
  Type selfType = getter->getImplicitSelfDecl()->getType();
  Expr *selfArg = DerivedConformance::createSelfDeclRef(getter);
  selfArg->setType(selfType);

  // The builtin call gives us a Builtin.Executor.
  auto builtinCall =
      DerivedConformance::createBuiltinCall(ctx,
                          BuiltinValueKind::BuildDistributedActorIdentity,
                                            {selfType}, {}, {selfArg});

//  // Turn that into an UnownedSerialExecutor.
//  auto initCall = constructUnownedSerialExecutor(ctx, builtinCall);
//  if (!initCall) return failure();
//
//  auto ret = new (ctx) ReturnStmt(SourceLoc(), initCall, /*implicit*/ true);
  auto ret = new (ctx) ReturnStmt(SourceLoc(), builtinCall, /*implicit*/ true);

  auto body = BraceStmt::create(
      ctx, SourceLoc(), { ret }, SourceLoc(), /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

static std::pair<BraceStmt *, bool>
deriveBodyDistributedActor_actorTransport(AbstractFunctionDecl *getter, void *) {
  // var actorTransport: ActorTransport {
  //   get {
  //     return Builtin.buildDistributedActorTransport(self)
  //   }
  // }
  ASTContext &ctx = getter->getASTContext();

//  // Produce an empty brace statement on failure.
//  auto failure = [&]() -> std::pair<BraceStmt *, bool> {
//    auto body = BraceStmt::create(
//        ctx, SourceLoc(), { }, SourceLoc(), /*implicit=*/true);
//    return { body, /*isTypeChecked=*/true };
//  };

  // Build a reference to self.
  Type selfType = getter->getImplicitSelfDecl()->getType();
  Expr *selfArg = DerivedConformance::createSelfDeclRef(getter);
  selfArg->setType(selfType);

  // The builtin call gives us a Builtin.Executor.
  auto builtinCall =
      DerivedConformance::createBuiltinCall(ctx,
                          BuiltinValueKind::BuildDistributedActorTransport,
                                            {selfType}, {}, {selfArg});

  auto ret = new (ctx) ReturnStmt(SourceLoc(), builtinCall, /*implicit*/ true);

  auto body = BraceStmt::create(
      ctx, SourceLoc(), { ret }, SourceLoc(), /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

/// Adds the following, fairly special, properties to each distributed actor:
/// - actorTransport
/// - id
// TODO(distributed): move to DerivedConformanceDistributedActor style, since those are indeed protocol witnesses
static void addImplicitDistributedActorProperties(ClassDecl *decl) {
  assert(decl->isDistributedActor());

  auto &C = decl->getASTContext();

  // FIXME(distributed, doug help?): enable synthesis of id, but it's hard because we can't store ActorIdentity because Hashable...
  //        - we also do not want to store AnyActorIdentity because that's a ton of pointers...
  //        - so we might have to return a "RawActorIndentity" or "Builtin.ActorIdentity" and implement the "Any"
  //          using delegation to runtime, in C++ to call the hash etc? This sounds terrible...
//  // ```
//  // nonisolated var id: AnyActorIdentity { <builtin> }
//  // ```
//  {
//    auto propertyType = C.getAnyActorIdentityDecl()->getDeclaredInterfaceType();
//    // TODO(distributed): diagnose if this type is missing, see: diag::concurrency_lib_missing
//
//    VarDecl *propDecl;
//    PatternBindingDecl *pbDecl;
//    std::tie(propDecl, pbDecl) = createStoredProperty(
//        decl, C,
//        VarDecl::Introducer::Var, C.Id_id,
//        propertyType, propertyType,
//        /*isStatic=*/false, /*isFinal=*/true);
//
//    propDecl->getAttrs().add(
//        new (C) SemanticsAttr(SEMANTICS_DISTRIBUTED_ACTOR,
//                              SourceLoc(), SourceRange(), /*implicit*/ true));
//    propDecl->getAttrs().add(
//        new (C) NonisolatedAttr(/*IsImplicit=*/true));
//
//    // Define the getter.
//    auto getterDecl = DerivedConformance::addGetterToReadOnlyDerivedProperty(
//        propDecl, propertyType);
//    getterDecl->setBodySynthesizer(&deriveBodyDistributedActor_id);
//
////    // Make the property implicitly final.
////    property->getAttrs().add(new (ctx) FinalAttr(/*IsImplicit=*/true));
////    if (property->getFormalAccess() == AccessLevel::Open)
////      property->overwriteAccess(AccessLevel::Public);
//
//    decl->addMember(propDecl);
//    decl->addMember(pbDecl);
//  }

  // ```
  // nonisolated var actorTransport: ActorTransport { <builtin> }
  // ```
  {
    auto propertyType = C.getActorTransportDecl()->getDeclaredInterfaceType();
    // TODO(distributed): diagnose if this type is missing, see: diag::concurrency_lib_missing

    VarDecl *propDecl;
    PatternBindingDecl *pbDecl;
    std::tie(propDecl, pbDecl) = createStoredProperty(
        decl, C,
        VarDecl::Introducer::Var, C.Id_actorTransport,
        propertyType, propertyType,
        /*isStatic=*/false, /*isFinal=*/true);

    propDecl->getAttrs().add(
        new (C) SemanticsAttr(SEMANTICS_DISTRIBUTED_ACTOR,
                              SourceLoc(), SourceRange(), /*implicit*/ true));
    propDecl->getAttrs().add(
        new (C) NonisolatedAttr(/*IsImplicit=*/true));

    // Define the getter.
    auto getterDecl = DerivedConformance::addGetterToReadOnlyDerivedProperty(
        propDecl, propertyType);
    getterDecl->setBodySynthesizer(&deriveBodyDistributedActor_actorTransport);

    decl->addMember(propDecl);
    decl->addMember(pbDecl);
  }
}

/******************************************************************************/
/*************************** _REMOTE_ FUNCTIONS *******************************/
/******************************************************************************/

/// Synthesizes the for `_remote_xxx` functions.
///
/// Create a stub body that emits a fatal error message.
static std::pair<BraceStmt *, bool>
synthesizeRemoteFuncStubBody(AbstractFunctionDecl *func, void *context) {
  auto distributedFunc = static_cast<AbstractFunctionDecl *>(context);
  auto classDecl = func->getDeclContext()->getSelfClassDecl();
  auto &ctx = func->getASTContext();
  auto &SM = ctx.SourceMgr;

  auto *staticStringDecl = ctx.getStaticStringDecl();
  auto staticStringType = staticStringDecl->getDeclaredInterfaceType();
  auto staticStringInit = ctx.getStringBuiltinInitDecl(staticStringDecl);

  auto *uintDecl = ctx.getUIntDecl();
  auto uintType = uintDecl->getDeclaredInterfaceType();
  auto uintInit = ctx.getIntBuiltinInitDecl(uintDecl);

  auto missingTransportDecl = ctx.getMissingDistributedActorTransport();
  assert(missingTransportDecl && "Could not locate '_missingDistributedActorTransport' function");

  // Create a call to _Distributed._missingDistributedActorTransport
  auto loc = func->getLoc();
  Expr *ref = new (ctx) DeclRefExpr(missingTransportDecl,
                                    DeclNameLoc(loc), /*Implicit=*/true);
  ref->setType(missingTransportDecl->getInterfaceType()
  ->removeArgumentLabels(1));

  llvm::SmallString<64> buffer;
  StringRef fullClassName = ctx.AllocateCopy(
      (classDecl->getModuleContext()->getName().str() +
      "." +
      classDecl->getName().str()).toStringRef(buffer));

  auto *className = new (ctx) StringLiteralExpr(fullClassName, loc,
                                                /*Implicit=*/true);
  className->setBuiltinInitializer(staticStringInit);
  assert(isa<ConstructorDecl>(className->getBuiltinInitializer().getDecl()));
  className->setType(staticStringType);

  auto *funcName = new (ctx) StringLiteralExpr(
      ctx.AllocateCopy(func->getName().getBaseName().getIdentifier().str()), loc,
      /*Implicit=*/true);
  funcName->setType(staticStringType);
  funcName->setBuiltinInitializer(staticStringInit);

  // Note: Sadly we cannot just rely on #function, #file, #line for the location
  // (MagicIdentifierLiteralExpr), of the call because the call is made from a thunk.
  // That thunk does not carry those info today although it could.
  //
  // Instead, we offer the location where the distributed func was declared.
  auto fileString = SM.getDisplayNameForLoc(distributedFunc->getStartLoc());
  auto *file = new (ctx) StringLiteralExpr(fileString, loc, /*Implicit=*/true);
  file->setType(staticStringType);
  file->setBuiltinInitializer(staticStringInit);

  auto startLineAndCol = SM.getPresumedLineAndColumnForLoc(distributedFunc->getStartLoc());
//  auto *line = new (ctx) MagicIdentifierLiteralExpr(
//      MagicIdentifierLiteralExpr::Line, loc, /*Implicit=*/true);
//  auto *line = new (ctx) IntegerLiteralExpr(startLineAndCol.first, loc,
//                                            /*implicit*/ true);
  auto *line = IntegerLiteralExpr::createFromUnsigned(ctx, startLineAndCol.first);
  line->setType(uintType);
  line->setBuiltinInitializer(uintInit);

//  auto *column = new (ctx) MagicIdentifierLiteralExpr(
//      MagicIdentifierLiteralExpr::Column, loc, /*Implicit=*/true);
  auto *column = IntegerLiteralExpr::createFromUnsigned(ctx, startLineAndCol.second);
  column->setType(uintType);
  column->setBuiltinInitializer(uintInit);

  auto *call = CallExpr::createImplicit(
      ctx, ref, { className, funcName, file, line, column }, {});
  call->setType(ctx.getNeverType());
  call->setThrows(false);

  SmallVector<ASTNode, 2> stmts;
  stmts.push_back(call); // something() -> Never
  // stmts.push_back(new (ctx) ReturnStmt(SourceLoc(), /*Result=*/nullptr)); // FIXME(distributed): this causes 'different types for return type: String vs. ()'
  auto body = BraceStmt::create(ctx, SourceLoc(), stmts, SourceLoc(),
                                /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

static Identifier makeRemoteFuncIdentifier(FuncDecl* func) {
  auto &C = func->getASTContext();
  auto localFuncName = func->getBaseIdentifier().str().str();
  auto remoteFuncIdent = C.getIdentifier("_remote_" + localFuncName);
  return remoteFuncIdent;
}

/// Create a remote stub for the passed in \c func.
/// The remote stub function is not user accessible and mirrors the API of
/// the local function. It is always throwing, async, and user-inaccessible.
///
/// ```
/// // func greet(name: String) { ... }
/// dynamic <access> func _remote_greet(name: String) async throws {
///     fatalError(...)
/// }
/// ```
///
/// and is intended to be replaced by a transport library by providing an
/// appropriate @_dynamicReplacement function.
static void addImplicitRemoteActorFunction(ClassDecl *decl, FuncDecl *func) {
  auto &C = decl->getASTContext();
  auto parentDC = decl;

  auto remoteFuncIdent = makeRemoteFuncIdentifier(func);

  auto params = ParameterList::clone(C, func->getParameters());
  auto genericParams = func->getGenericParams(); // TODO(distributed): also clone those
  Type resultTy = func->getResultInterfaceType();

  DeclName name(C, remoteFuncIdent, params);
  auto *const remoteFuncDecl = FuncDecl::createImplicit(
      C, StaticSpellingKind::None, name, /*NameLoc=*/SourceLoc(),
      /*Async=*/true, /*Throws=*/true,
      /*GenericParams=*/genericParams, params,
      resultTy, parentDC);

  // *dynamic* because we'll be replacing it with specific transports
  remoteFuncDecl->getAttrs().add(
      new (C) DynamicAttr(/*implicit=*/true));

  // @_distributedActorIndependent
  remoteFuncDecl->getAttrs().add(
      new (C) DistributedActorIndependentAttr(/*IsImplicit=*/true));

  // users should never have to access this function directly;
  // it is only invoked from our distributed function thunk if the actor is remote.
  remoteFuncDecl->setUserAccessible(false);
  remoteFuncDecl->setSynthesized();

  remoteFuncDecl->setBodySynthesizer(&synthesizeRemoteFuncStubBody, func);

  // same access control as the original function is fine
  remoteFuncDecl->copyFormalAccessFrom(func, /*sourceIsParentContext=*/false);

  decl->addMember(remoteFuncDecl);
}

/// Synthesize dynamic _remote stub functions for each encountered distributed function.
static void addImplicitRemoteActorFunctions(ClassDecl *decl) {
  assert(decl->isDistributedActor());

  for (auto member : decl->getMembers()) {
    auto func = dyn_cast<FuncDecl>(member);
    if (func && func->isDistributed()) {
      addImplicitRemoteActorFunction(decl, func);
    }
  }
}

/******************************************************************************/
/************************ SYNTHESIS ENTRY POINT *******************************/
/******************************************************************************/

/// Entry point for adding all computed members to a distributed actor decl.
void swift::addImplicitDistributedActorMembersToClass(ClassDecl *decl) {
  // Bail out if not a distributed actor definition.
  if (!decl->isDistributedActor())
    return;

  auto &C = decl->getASTContext();

  if (!C.getLoadedModule(C.Id_Distributed)) {
    // seems we're missing the _Distributed module, ask to import it explicitly
    decl->diagnose(diag::distributed_actor_needs_explicit_distributed_import);
    return;
  }

  addImplicitDistributedActorConstructors(decl);
  addImplicitDistributedActorProperties(decl);
  addImplicitRemoteActorFunctions(decl);
//  addImplicitResignIdentity(decl);
}
