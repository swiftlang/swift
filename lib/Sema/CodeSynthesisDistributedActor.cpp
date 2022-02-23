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
#include "swift/AST/DistributedDecl.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "DerivedConformances.h"
using namespace swift;


/******************************************************************************/
/************************ PROPERTY SYNTHESIS **********************************/
/******************************************************************************/

// Note: This would be nice to implement in DerivedConformanceDistributedActor,
// but we can't since those are lazily triggered and an implementation exists
// for the 'id' property because 'Identifiable.id' has an extension that impls
// it for ObjectIdentifier, and we have to instead emit this stored property.
//
// The "derived" mechanisms are not really geared towards emitting for
// what already has a witness.
static VarDecl *addImplicitDistributedActorIDProperty(
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
/*********************** DISTRIBUTED THUNK SYNTHESIS **************************/
/******************************************************************************/

static void forwardParameters(AbstractFunctionDecl *afd,
                              SmallVectorImpl<Argument> &forwardingParams) {
  auto &C = afd->getASTContext();
  auto params = afd->getParameters();

  for (auto param : *params) {
    Expr *paramRefExpr =
        new (C) DeclRefExpr(param, DeclNameLoc(), /*implicit=*/true);
    paramRefExpr->setType(param->getType());

    auto arg = Argument(SourceLoc(), param->getArgumentName(), paramRefExpr);
    forwardingParams.push_back(arg);
  }
}

static std::pair<BraceStmt *, bool>
deriveBodyDistributed_thunk(AbstractFunctionDecl *thunk, void *context) {
  auto implicit = true;
  ASTContext &C = thunk->getASTContext();
  DeclContext *DC = thunk->getDeclContext();
  auto module = thunk->getModuleContext();

  // mock locations, we're a thunk and don't really need detailed locations
  const SourceLoc sloc = SourceLoc();
  const DeclNameLoc dloc = DeclNameLoc();

  auto func = static_cast<FuncDecl *>(context);
  assert(func && func->isDistributed() && "base for distributed thunk must be 'distributed'");
  auto funcDC = func->getDeclContext();
  assert(funcDC && "Function must be part of distributed actor");
  NominalTypeDecl *nominal = funcDC->getSelfNominalTypeDecl();
  assert(nominal && nominal->isDistributedActor() && "Function must be part of distributed actor");

  // === self
  auto selfDecl = thunk->getImplicitSelfDecl();
  auto selfRefExpr = new (C) DeclRefExpr(selfDecl, dloc, implicit);

  // === return type
  Type returnTy = func->getResultInterfaceType();
  auto isVoidReturn = returnTy->isVoid();

  // === self.actorSystem
  ProtocolDecl *DAS = C.getDistributedActorSystemDecl();
  Type systemTy = getConcreteReplacementForProtocolActorSystemType(thunk);
  assert(systemTy);
  NominalTypeDecl *systemDecl = systemTy->getAnyNominal();
  assert(systemDecl);
  auto systemConfRef = module->lookupConformance(systemTy, DAS);
  assert(systemConfRef && "ActorSystem must conform to DistributedActorSystem");

  auto systemProperty = nominal->getDistributedActorSystemProperty();
  // auto systemPropRef = systemConfRef.getConcrete()->getWitnessDecl() // could handle via protocol?
  auto systemRefExpr = new (C)
      MemberRefExpr(selfRefExpr, sloc, ConcreteDeclRef(systemProperty),
                    dloc, implicit);

  // === ActorSystem.InvocationEncoder
  ProtocolDecl *DTIE = C.getDistributedTargetInvocationEncoderDecl();
  Type invocationEncoderTy =
      getDistributedActorSystemInvocationEncoderType(systemDecl);
  NominalTypeDecl *invocationEncoderDecl = invocationEncoderTy->getAnyNominal();

  // === __isRemoteActor(self)
  ArgumentList *isRemoteArgs =
      ArgumentList::createImplicit(C, {});

  FuncDecl *isRemoteFn = C.getIsRemoteDistributedActor();
  assert(isRemoteFn &&
         "Could not find 'is remote' function, is the '_Distributed' module available?");
  auto isRemoteDeclRef =
      UnresolvedDeclRefExpr::createImplicit(C, isRemoteFn->getName());
  auto isRemote =
      CallExpr::createImplicit(C, isRemoteDeclRef, isRemoteArgs);

  // === local branch ----------------------------------------------------------
  // -- forward arguments
  SmallVector<Argument, 8> forwardingParams;
  forwardParameters(func, forwardingParams);
  auto forwardingArgList = ArgumentList::createImplicit(C, forwardingParams);

//  auto localFuncDotCall = DotSyntaxCallExpr::create(
//      C, UnresolvedDeclRefExpr::createImplicit(C, func->getName()),
//      sloc, selfRefExpr);
//  localFuncDotCall->dump();
  auto localCallSubs = SubstitutionMap();
  auto funcDeclRef = new (C) DeclRefExpr(ConcreteDeclRef(func, localCallSubs),
                                         dloc, implicit);
  funcDeclRef->setType(func->getInterfaceType());
//  auto localFuncCall = new (C) CallExpr(localFuncDotCall, forwardingArgList);
  fprintf(stderr, "[%s:%d] (%s) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", __FILE__, __LINE__, __FUNCTION__);
  funcDeclRef->dump();
  auto dotFuncDeclRef = DotSyntaxCallExpr::create(C, funcDeclRef, sloc, selfRefExpr);
  fprintf(stderr, "[%s:%d] (%s) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", __FILE__, __LINE__, __FUNCTION__);
  auto localFuncCall = CallExpr::createImplicit(C, dotFuncDeclRef, forwardingArgList);
  auto localResult = new (C) AwaitExpr(sloc, localFuncCall);
  auto localReturn = new (C) ReturnStmt(sloc, localResult, implicit);
  auto localBranchStmt = 
      BraceStmt::create(C, sloc, {localReturn}, sloc, implicit);

  // === remote branch  --------------------------------------------------------
  SmallVector<ASTNode, 8> remoteBranchStmts;

  // --- invocationEncoder = system.makeInvocationEncoder()
  VarDecl *invocationVar =
      new (C) VarDecl(/*IsStatic*/ false, VarDecl::Introducer::Var, sloc,
                      C.getIdentifier("invocationEncoder"), DC);
  invocationVar->setInterfaceType(invocationEncoderTy);
  invocationVar->setImplicit();
  invocationVar->setSynthesized();

  Pattern *invocationPattern = NamedPattern::createImplicit(C, invocationVar);
  invocationPattern->setType(invocationEncoderTy);

//  invocationPattern = TypedPattern::createImplicit(C, invocationPattern);
//  invocationPattern->setType(invocationEncoderTy);

  FuncDecl *makeInvocationEncoderDecl =
      C.getMakeInvocationEncoderOnDistributedActorSystem(func);
  auto makeInvocationRef = new (C) DeclRefExpr(makeInvocationEncoderDecl, dloc, implicit);
  makeInvocationRef->setType(invocationEncoderTy);

  auto makeInvocationDotExpr =
      DotSyntaxCallExpr::create(C, makeInvocationRef, sloc, systemRefExpr);
//  auto makeInvocationDotExpr =
//      UnresolvedDotExpr::createImplicit(C, systemRefExpr, makeInvocationEncoderDecl->getName());
  auto *makeInvocationArgs = ArgumentList::createImplicit(C, {});
  auto makeInvocationCallExpr =
      CallExpr::createImplicit(C, makeInvocationDotExpr, makeInvocationArgs);
  makeInvocationCallExpr->setType(DTIE->getInterfaceType());
  makeInvocationCallExpr->setThrows(false);

  auto invocationEncoderPB = PatternBindingDecl::createImplicit(
          C, StaticSpellingKind::None, invocationPattern, makeInvocationCallExpr, DC);
  remoteBranchStmts.push_back(invocationEncoderPB);

  // --- Recording invocation details
  // -- recordArgument
  {
    auto recordArgumentDecl =
        C.getRecordArgumentOnDistributedInvocationEncoder(invocationEncoderDecl);

    for (auto param : *thunk->getParameters()) {
      fprintf(stderr, "[%s:%d] (%s) FORWARD PARAM\n", __FILE__, __LINE__,
              __FUNCTION__);
      param->dump();
    }
  }

  // -- recordErrorType
  if (func->hasThrows()) {
    // TODO:
    auto recordErrorDecl = C.getRecordErrorTypeOnDistributedInvocationEncoder(
        invocationEncoderDecl);
    auto recordErrorDeclRef = new (C)
        DeclRefExpr(ConcreteDeclRef(recordErrorDecl), dloc, implicit);
//    auto recordErrorStmt = new (C) TryExpr(
//        sloc, DotSyntaxCallExpr::create(C, recordErrorDeclRef,
//                                               sloc, invocationVar));
  }

  // -- recordReturnType
  {
    if (!isVoidReturn) {
      assert(false && "not implemented nonvoid");
    }
  }

  // -- doneRecording
  {
    auto doneRecordingDecl =
        C.getDoneRecordingOnDistributedInvocationEncoder(invocationEncoderDecl);

//    auto tryCallDoneRecording =
//      new (C) TryExpr(sloc, DotSyntaxCallExpr::create(C, new (C) CallExpr()))
  }

  // === Make the 'remoteCall(Void)'
  {
    auto remoteCallDecl =
        C.getRemoteCallOnDistributedActorSystem(systemDecl, isVoidReturn);

//    auto awaitRemoteCallExpr = new (C) AwaitExpr(sloc, );
//    auto tryAwaitRemoteCalLExpr = new (C) TryExpr(sloc, )
  }


  // ---------------------------------------------------------------------------
  auto remoteBranchStmt =
      BraceStmt::create(C, sloc, remoteBranchStmts, sloc, implicit);

  // ---------------------------------------------------------------------------
  // === if (isRemote(...) <remote branch> else <local branch>
  auto ifStmt = new (C) IfStmt(sloc,
                             isRemote,
                             /*then=*/remoteBranchStmt, sloc,
                             /*else=*/localBranchStmt, implicit, C);

  auto body = BraceStmt::create(C, sloc, {ifStmt}, sloc, implicit);

  fprintf(stderr, "[%s:%d] (%s) BODY ====== BODY ====== BODY ====== BODY ====== BODY ====== \n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) BODY ====== BODY ====== BODY ====== BODY ====== BODY ====== \n", __FILE__, __LINE__, __FUNCTION__);
  body->dump();
  fprintf(stderr, "[%s:%d] (%s) BODY ====== BODY ====== BODY ====== BODY ====== BODY ====== \n", __FILE__, __LINE__, __FUNCTION__);
  return {body, /*isTypeChecked=*/false};
}

static FuncDecl *createDistributedThunkFunction(FuncDecl *func) {
  auto &C = func->getASTContext();
  auto DC = func->getDeclContext();
  auto module = func->getParentModule();
  NominalTypeDecl *nominal;
  if (auto actor = dyn_cast<NominalTypeDecl>(DC)) {
    nominal = actor;
  } else {
    nominal = func->getSelfNominalTypeDecl();
  }
  assert(nominal);

  auto systemTy = getConcreteReplacementForProtocolActorSystemType(func);
  if (!systemTy)
    assert(false);

  std::string thunkNameString = "_DIST_";
  thunkNameString.append(std::string(func->getBaseName().getIdentifier().str()));
  auto thunkBaseName = DeclBaseName(C.getIdentifier(StringRef(thunkNameString)));
  DeclName thunkName = DeclName(C, thunkBaseName, func->getParameters());

  GenericParamList *genericParamList = nullptr;
  if (auto genericParams = func->getGenericParams()) {
    genericParamList = genericParams->clone(DC);
  }
  ParameterList *params = func->getParameters()->clone(C);

  auto thunk = FuncDecl::createImplicit(C, swift::StaticSpellingKind::None,
                           thunkName, SourceLoc(),
                           /*async=*/true, /*throws=*/true,
                           genericParamList,
                           params,
                           func->getResultInterfaceType(),
                           DC);
  thunk->setSynthesized(true);
  thunk->getAttrs().add(new (C) NonisolatedAttr(/*implicit=*/true));
  thunk->setBodySynthesizer(deriveBodyDistributed_thunk, func);
  thunk->setGenericSignature(func->getGenericSignature());
  thunk->copyFormalAccessFrom(func, /*sourceIsParentContext=*/false);

  fprintf(stderr, "[%s:%d] (%s) THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK \n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK \n", __FILE__, __LINE__, __FUNCTION__);
  thunk->dump();
  fprintf(stderr, "[%s:%d] (%s) THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK THUNK \n", __FILE__, __LINE__, __FUNCTION__);

  return thunk;
}

/******************************************************************************/
/************************ SYNTHESIS ENTRY POINT *******************************/
/******************************************************************************/

FuncDecl *GetDistributedThunkRequest::evaluate(
    Evaluator &evaluator, AbstractFunctionDecl *afd) const {
  if (!afd->isDistributed())
    return nullptr;

  auto &C = afd->getASTContext();
  auto DC = afd->getDeclContext();
  if (isa<ProtocolDecl>(DC->getSelfNominalTypeDecl())) {
    // we don't synthesize thunks
    return nullptr;
  }

  if (auto func = dyn_cast<FuncDecl>(afd)) {

    // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
    // we won't be emitting the offending decl after all.
    if (!C.getLoadedModule(C.Id_Distributed))
      return nullptr;

    return createDistributedThunkFunction(func);
  }

  llvm_unreachable("Unable to synthesize distributed thunk");
}

VarDecl *GetDistributedActorIDPropertyRequest::evaluate(
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


VarDecl *GetDistributedActorSystemPropertyRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *actor) const {
  auto &C = actor->getASTContext();
  auto module = actor->getParentModule();

  auto sys = C.getProtocol(KnownProtocolKind::DistributedActorSystem);
  sys->dump();
  auto f = sys->lookupDirect(C.Id_makeInvocationEncoder);

  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  auto classDecl = dyn_cast<ClassDecl>(actor);
  if (!classDecl)
    return nullptr;

  if (!actor->isDistributedActor())
    return nullptr;

  auto expectedSystemType = getDistributedActorSystemType(classDecl);
//  fprintf(stderr, "[%s:%d] (%s) expectedSystemType\n", __FILE__, __LINE__, __FUNCTION__);
//  expectedSystemType.dump();
  auto DistSystemProtocol =
      C.getProtocol(KnownProtocolKind::DistributedActorSystem);
//  fprintf(stderr, "[%s:%d] (%s) DistSystemProtocol\n", __FILE__, __LINE__, __FUNCTION__);
//  DistSystemProtocol->dump();

  if (auto proto = dyn_cast<ProtocolDecl>(actor)) {
    fprintf(stderr, "[%s:%d] (%s) protocol!\n", __FILE__, __LINE__, __FUNCTION__);
    proto->dump();

    if (auto systemAssocTyDecl = proto->getAssociatedType(C.Id_ActorSystem)) {
      assert(false && "handling assoc type in protocol not handled");
    }

    auto DistributedActorProto = C.getDistributedActorDecl();
    for (auto system : DistributedActorProto->lookupDirect(C.Id_actorSystem, {})) {
      if (auto var = dyn_cast<VarDecl>(system)) {
        auto conformance = module->conformsToProtocol(var->getInterfaceType(),
                                                      DistSystemProtocol);

        if (conformance.isInvalid())
          continue;

        return var;
      }
    }

    return nullptr;
  }

  for (auto system : actor->lookupDirect(C.Id_actorSystem)) {
    if (auto var = dyn_cast<VarDecl>(system)) {
      auto conformance = module->conformsToProtocol(var->getInterfaceType(),
                                                    DistSystemProtocol);
      if (conformance.isInvalid())
        continue;

      return var;
    }
  }

  return nullptr;
}
