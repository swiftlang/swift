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

/// Synthesizes an empty body of the `init(transport:)` initializer as:
///
/// ```
/// init(transport: ActorTransport) {
///   <filled in by SILGenDistributed>
/// }
/// ```
///
/// \param initDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
createBody_DistributedActor_init_default(AbstractFunctionDecl *initDecl, void *) {
  auto *funcDC = cast<DeclContext>(initDecl);
  ASTContext &C = funcDC->getASTContext();

  // We will treat this constructor just as any other local init in SILGen
  // and inject code that performs the id and transport initialization.
  // Here in Sema though it has to be
  auto *body = BraceStmt::create(
      C, SourceLoc(),
      {new (C) ReturnStmt(SourceLoc(), nullptr, /*Implicit=*/true)},
      SourceLoc(), /*implicit=*/true);

  // Ignore the empty body in type checking;
  // We synthesize the implementation in SILGenDistributed
  return { body, /*isTypeChecked=*/true };
}

/// Synthesizes the
///
/// ```
/// init(transport: ActorTransport)
/// ```
///
/// local initializer.
static ConstructorDecl *
createDistributedActor_init_default(ClassDecl *classDecl,
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
  initDecl->setBodySynthesizer(&createBody_DistributedActor_init_default);

  auto *nonIsoAttr = new (C) NonisolatedAttr(/*IsImplicit*/true);
  initDecl->getAttrs().add(nonIsoAttr);

  initDecl->copyFormalAccessFrom(classDecl, /*sourceIsParentContext=*/true);

  // Clone any @available attributes from the actor definition.
  for (auto attr: classDecl->getAttrs().getAttributes<AvailableAttr>())
    initDecl->getAttrs().add(attr->clone(C, /*implicit*/true));

  return initDecl;
}

static void addImplicitDefaultDistributedActorConstructor(ClassDecl *decl) {
  fprintf(stderr, "[%s:%d] (%s) addImplicitDefaultDistributedActorConstructor\n", __FILE__, __LINE__, __FUNCTION__);
  decl->setAddedImplicitInitializers();

  // Check whether the user has defined a designated initializer for this actor,
  // if so, we will not synthesize the init(transport:) initializer.
  bool foundDesignatedInit = decl->hasUserDefinedDesignatedInit();
  if (foundDesignatedInit) {
    fprintf(stderr, "[%s:%d] (%s) FOUND DESIGNATED INIT SKIP SYNTHESIS\n", __FILE__, __LINE__, __FUNCTION__);
    decl->dump();
    return;
  }

  fprintf(stderr, "[%s:%d] (%s) MAKE DESIGNATED INIT SYNTHESIS\n", __FILE__, __LINE__, __FUNCTION__);
  auto &ctx = decl->getASTContext();
  auto init = createDistributedActor_init_default(decl, ctx);
  decl->addMember(init);
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
  SmallVector<ASTNode, 2> statements; // TODO: how to init at body statements count size?
  for (auto stmt : body->getElements())
    statements.push_back(stmt); // TODO: copy?

  // TODO: INJECT THIS AS FIRST THING IN A DEFER {}

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

  deinitDecl->setBody(newBody, AbstractFunctionDecl::BodyKind::TypeChecked); // FIXME: no idea if Parsed is right, we are NOT type checked I guess?
}

/******************************************************************************/
/******************************** PROPERTIES **********************************/
/******************************************************************************/

// TODO: deduplicate with 'declareDerivedProperty' from DerivedConformance...
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

/// Adds the following, fairly special, properties to each distributed actor:
/// - actorTransport
/// - id
static void addImplicitDistributedActorStoredProperties(ClassDecl *decl) {
  assert(decl->isDistributedActor());

  auto &C = decl->getASTContext();

  // ```
  // @_distributedActorIndependent
  // let id: AnyActorIdentity // TODO: move to `nonisolated var id {}` once we have the new allocation scheme
  // ```
  {
    auto propertyType = C.getAnyActorIdentityDecl()->getDeclaredInterfaceType();

    VarDecl *propDecl;
    PatternBindingDecl *pbDecl;
    std::tie(propDecl, pbDecl) = createStoredProperty(
        decl, C,
        VarDecl::Introducer::Let, C.Id_id,
        propertyType, propertyType,
        /*isStatic=*/false, /*isFinal=*/true);

    // mark as @_distributedActorIndependent, allowing access to it from everywhere
    propDecl->getAttrs().add(
        new (C) DistributedActorIndependentAttr(/*IsImplicit=*/true)); // TODO: remove and move to nonisolated once new constructors land

    decl->addMember(propDecl);
    decl->addMember(pbDecl);
  }

  // ```
  // @_distributedActorIndependent
  // let actorTransport: ActorTransport
  // ```
  // (no need for @actorIndependent because it is an immutable let)
  {
    auto propertyType = C.getActorTransportDecl()->getDeclaredInterfaceType();

    VarDecl *propDecl;
    PatternBindingDecl *pbDecl;
    std::tie(propDecl, pbDecl) = createStoredProperty(
        decl, C,
        VarDecl::Introducer::Let, C.Id_actorTransport,
        propertyType, propertyType,
        /*isStatic=*/false, /*isFinal=*/true);

    // mark as @_distributedActorIndependent, allowing access to it from everywhere
    propDecl->getAttrs().add(
        new (C) DistributedActorIndependentAttr(/*IsImplicit=*/true)); // TODO: remove and move to nonisolated once new constructors land

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
  // stmts.push_back(new (ctx) ReturnStmt(SourceLoc(), /*Result=*/nullptr)); // FIXME: this causes 'different types for return type: String vs. ()'
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
  auto genericParams = func->getGenericParams(); // TODO: also clone those
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

  // === Ensure the _Distributed module is loaded
  auto &C = decl->getASTContext();
  if (!C.getLoadedModule(C.Id_Distributed)) {
    // seems we're missing the _Distributed module, ask to import it explicitly
    decl->diagnose(diag::distributed_actor_needs_explicit_distributed_import);
    return;
  }

  addImplicitDefaultDistributedActorConstructor(decl);
  addImplicitDistributedActorStoredProperties(decl);
  addImplicitRemoteActorFunctions(decl);
//  addImplicitResignIdentity(decl);
}
