//===--- CodeSynthesisDistributedActor.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TypeCheckDistributed.h"


#include "TypeChecker.h"
#include "TypeCheckType.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Availability.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "DerivedConformances.h"
using namespace swift;

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

  auto missingTransportDecl = ctx.getMissingDistributedActorSystem();
  assert(missingTransportDecl && "Could not locate '_missingDistributedActorSystem' function");

  // Create a call to _Distributed._missingDistributedActorSystem
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
  auto *line = IntegerLiteralExpr::createFromUnsigned(ctx, startLineAndCol.first);
  line->setType(uintType);
  line->setBuiltinInitializer(uintInit);

  auto *column = IntegerLiteralExpr::createFromUnsigned(ctx, startLineAndCol.second);
  column->setType(uintType);
  column->setBuiltinInitializer(uintInit);

  auto *argList = ArgumentList::forImplicitUnlabeled(
      ctx, {className, funcName, file, line, column});
  auto *call = CallExpr::createImplicit(ctx, ref, argList);
  call->setType(ctx.getNeverType());
  call->setThrows(false);

  SmallVector<ASTNode, 2> stmts;
  stmts.push_back(call);
  auto body = BraceStmt::create(ctx, SourceLoc(), stmts, SourceLoc(),
                                /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

static Identifier makeRemoteFuncIdentifier(FuncDecl* distributedFunc) {
  auto &C = distributedFunc->getASTContext();
  assert(distributedFunc->isDistributed());
  auto localFuncName = distributedFunc->getBaseIdentifier().str().str();
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
static AbstractFunctionDecl *addImplicitDistributedActorRemoteFunction(
    DeclContext *parentDC, AbstractFunctionDecl *AFD) {
  auto nominal = parentDC->getSelfNominalTypeDecl();
  if (!nominal || !nominal->isDistributedActor())
    return nullptr;

  auto func = dyn_cast<FuncDecl>(AFD);
  if (!func || !func->isDistributed())
    return nullptr;

  // ==== if the remote func already exists, return it
  if (auto existing = nominal->lookupDirectRemoteFunc(func))
    return existing;

  // ==== Synthesize and add 'remote' func to the actor decl

  auto &C = func->getASTContext();
  auto remoteFuncIdent = makeRemoteFuncIdentifier(func);

  auto params = ParameterList::clone(C, func->getParameters());
  auto genericParams = func->getGenericParams(); // TODO(distributed): also clone those?
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

  // nonisolated
  remoteFuncDecl->getAttrs().add(new (C) NonisolatedAttr(/*IsImplicit=*/true));

  // nonisolated
  remoteFuncDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  // users should never have to access this function directly;
  // it is only invoked from our distributed instance method thunk if the actor is remote.
  remoteFuncDecl->setUserAccessible(false);
  remoteFuncDecl->setSynthesized();

  remoteFuncDecl->setBodySynthesizer(&synthesizeRemoteFuncStubBody, func);

  // same access control as the original function is fine
  remoteFuncDecl->copyFormalAccessFrom(func, /*sourceIsParentContext=*/false);

  // add the func to the context:
  cast<IterableDeclContext>(parentDC->getAsDecl())->addMember(remoteFuncDecl);

  return remoteFuncDecl;
}

// Note: This would be nice to implement in DerivedConformanceDistributedActor,
// but we can't since those are lazily triggered and an implementation exists
// for the 'id' property because 'Identifiable.id' has an extension that impls
// it for ObjectIdentifier, and we have to instead emit this stored property.
//
// The "derived" mechanisms are not really geared towards emitting for
// what already has a witness.
static ValueDecl *addImplicitDistributedActorIDProperty(
//    DeclContext *parentDC,
    NominalTypeDecl *nominal) {
  if (!nominal || !nominal->isDistributedActor())
    return nullptr;

  // ==== if the 'id' already exists, return it
  auto &C = nominal->getASTContext();
//  if (auto existing = nominal->lookupDirect(C.Id_id))
//    return existing;

  // ==== Synthesize and add 'id' property to the actor decl
  Type propertyType = getDistributedActorIDType(nominal);

  VarDecl *propDecl = new (C)
      VarDecl(/*IsStatic*/false, VarDecl::Introducer::Let,
              SourceLoc(), C.Id_id, nominal);
  propDecl->setImplicit();
  propDecl->setSynthesized();
  propDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  propDecl->setInterfaceType(propertyType);

  Pattern *propPat = NamedPattern::createImplicit(C, propDecl);
  propPat->setType(propertyType);

  propPat = TypedPattern::createImplicit(C, propPat, propertyType);
  propPat->setType(propertyType);

  PatternBindingDecl *pbDecl = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, propPat, /*InitExpr*/ nullptr,
      nominal);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  nominal->addMember(propDecl);
  nominal->addMember(pbDecl);

  return propDecl;
}

/******************************************************************************/
/************************ SYNTHESIS ENTRY POINT *******************************/
/******************************************************************************/

AbstractFunctionDecl *GetDistributedRemoteFuncRequest::evaluate(
    Evaluator &evaluator, AbstractFunctionDecl *func) const {
  if (!func->isDistributed())
    return nullptr;

  auto &C = func->getASTContext();
  DeclContext *DC = func->getDeclContext();

  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  return addImplicitDistributedActorRemoteFunction(DC, func);
}

ValueDecl *GetDistributedActorIDPropertyRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *actor) const {
  if (!actor->isDistributedActor())
    return nullptr;

  auto &C = actor->getASTContext();

  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  return addImplicitDistributedActorIDProperty(actor);
}
