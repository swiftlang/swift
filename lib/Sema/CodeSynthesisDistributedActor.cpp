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
#include "swift/AST/ASTMangler.h"
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
  auto module = thunk->getParentModule();

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
  assert(systemTy && "distributed thunk can only be synthesized for with actor system types");
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

  // === Type:
  StructDecl *RCT = C.getRemoteCallTargetDecl();
  Type remoteCallTargetTy = RCT->getDeclaredInterfaceType();

  // === __isRemoteActor(self)
  ArgumentList *isRemoteArgs =
      ArgumentList::forImplicitSingle(C, /*label=*/Identifier(), selfRefExpr);

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

  auto localCallSubs = SubstitutionMap();
  // auto funcDeclRef = UnresolvedDeclRefExpr::createImplicit(C, func->getBaseName());
  auto funcDeclRef =
      UnresolvedDotExpr::createImplicit(C, selfRefExpr, func->getBaseName());
  Expr *localFuncCall = CallExpr::createImplicit(C, funcDeclRef, forwardingArgList);
  localFuncCall = AwaitExpr::createImplicit(C, sloc, localFuncCall);
  if (func->hasThrows()) {
    localFuncCall = TryExpr::createImplicit(C, sloc, localFuncCall);
  }
  auto returnLocalFuncCall = new (C) ReturnStmt(sloc, localFuncCall, implicit);
  auto localBranchStmt = 
      BraceStmt::create(C, sloc, {returnLocalFuncCall}, sloc, implicit);

  // === remote branch  --------------------------------------------------------
  SmallVector<ASTNode, 8> remoteBranchStmts;

  // --- invocationEncoder = system.makeInvocationEncoder()
  VarDecl *invocationVar =
      new (C) VarDecl(/*isStatic=*/false, VarDecl::Introducer::Var, sloc,
                      C.getIdentifier("__invocationEncoder"), thunk);
  invocationVar->setInterfaceType(invocationEncoderTy);
  invocationVar->setImplicit();
  invocationVar->setSynthesized();

  {
    Pattern *invocationPattern = NamedPattern::createImplicit(C, invocationVar);

    FuncDecl *makeInvocationEncoderDecl =
        C.getMakeInvocationEncoderOnDistributedActorSystem(func);
    auto makeInvocationExpr = UnresolvedDotExpr::createImplicit(
        C, systemRefExpr, makeInvocationEncoderDecl->getName());
    auto *makeInvocationArgs = ArgumentList::createImplicit(C, {});
    auto makeInvocationCallExpr =
        CallExpr::createImplicit(C, makeInvocationExpr, makeInvocationArgs);
    makeInvocationCallExpr->setType(DTIE->getInterfaceType());
    makeInvocationCallExpr->setThrows(false);

    auto invocationEncoderPB = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, invocationPattern, makeInvocationCallExpr,
        thunk);
    remoteBranchStmts.push_back(invocationEncoderPB);
    remoteBranchStmts.push_back(invocationVar);
  }

  // --- Recording invocation details
  // -- recordArgument
  {
    auto recordArgumentDecl =
        C.getRecordArgumentOnDistributedInvocationEncoder(invocationEncoderDecl);
    assert(recordArgumentDecl);
    auto recordArgumentDeclRef =
        UnresolvedDeclRefExpr::createImplicit(C, recordArgumentDecl->getName());

    for (auto param : *thunk->getParameters()) {
      fprintf(stderr, "[%s:%d] (%s) CONTEXT OF PARAM:\n", __FILE__, __LINE__, __FUNCTION__);
      param->getDeclContext()->getAsDecl()->dump();
      fprintf(stderr, "[%s:%d] (%s) \n", __FILE__, __LINE__, __FUNCTION__);
      auto recordArgArgsList = ArgumentList::forImplicitCallTo(
          recordArgumentDeclRef->getName(),
          {
            new (C) DeclRefExpr(
               ConcreteDeclRef(param), dloc, implicit,
               AccessSemantics::Ordinary,
               thunk->mapTypeIntoContext(param->getInterfaceType()))
          }, C);
      auto tryRecordArgExpr = TryExpr::createImplicit(C, sloc,
        CallExpr::createImplicit(C,
          UnresolvedDotExpr::createImplicit(C,
          // UnresolvedDeclRefExpr::createImplicit(C, invocationVar->getName()),
          new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc, implicit, AccessSemantics::Ordinary),
          recordArgumentDecl->getName()),
          recordArgArgsList));

      remoteBranchStmts.push_back(tryRecordArgExpr);
    }
  }

  // Error.self
  auto errorDecl = C.getErrorDecl();
  auto *errorTypeExpr = new (C) DotSelfExpr(
      UnresolvedDeclRefExpr::createImplicit(C, errorDecl->getName()), sloc,
      sloc, errorDecl->getDeclaredInterfaceType());

  // Never.self
  auto neverDecl = C.getNeverDecl();
  auto *neverTypeExpr = new (C) DotSelfExpr(
      UnresolvedDeclRefExpr::createImplicit(C, errorDecl->getName()), sloc,
      sloc, errorDecl->getDeclaredInterfaceType());

  // -- recordErrorType
  if (func->hasThrows()) {
    auto recordErrorDecl = C.getRecordErrorTypeOnDistributedInvocationEncoder(
        invocationEncoderDecl);
    assert(recordErrorDecl);
    auto recordErrorDeclRef =
        UnresolvedDeclRefExpr::createImplicit(C, recordErrorDecl->getName());

    auto recordArgsList = ArgumentList::forImplicitCallTo(
        recordErrorDeclRef->getName(),
        {errorTypeExpr},
        C);
    auto tryRecordErrorTyExpr = TryExpr::createImplicit(C, sloc,
      CallExpr::createImplicit(C,
        UnresolvedDotExpr::createImplicit(C,
//          UnresolvedDeclRefExpr::createImplicit(C,invocationVar->getName()),
          new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc, implicit, AccessSemantics::Ordinary),
          recordErrorDecl->getName()),
        recordArgsList));

    remoteBranchStmts.push_back(tryRecordErrorTyExpr);
  }

  // Result.self
  auto resultType = func->getResultInterfaceType();
  auto *metaTypeRef = TypeExpr::createImplicit(resultType, C);
  auto *resultTypeExpr =
      new (C) DotSelfExpr(metaTypeRef, sloc, sloc, resultType);

  // -- recordReturnType
  if (!isVoidReturn) {
    auto recordReturnTypeDecl =
        C.getRecordReturnTypeOnDistributedInvocationEncoder(
            invocationEncoderDecl);
    auto recordReturnTypeDeclRef =
        UnresolvedDeclRefExpr::createImplicit(C, recordReturnTypeDecl->getName());

    auto recordArgsList = ArgumentList::forImplicitCallTo(
        recordReturnTypeDeclRef->getName(),
        {resultTypeExpr},
        C);
    auto tryRecordReturnTyExpr = TryExpr::createImplicit(C, sloc,
      CallExpr::createImplicit(C,
        UnresolvedDotExpr::createImplicit(C,
//          UnresolvedDeclRefExpr::createImplicit(C,invocationVar->getName()),
          new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc, implicit, AccessSemantics::Ordinary),
          recordReturnTypeDecl->getName()),
        recordArgsList));

    remoteBranchStmts.push_back(tryRecordReturnTyExpr);
  }

  // -- doneRecording
  {
    auto doneRecordingDecl =
        C.getDoneRecordingOnDistributedInvocationEncoder(invocationEncoderDecl);
    auto doneRecordingDeclRef =
        UnresolvedDeclRefExpr::createImplicit(C, doneRecordingDecl->getName());

    auto argsList =
        ArgumentList::forImplicitCallTo(doneRecordingDeclRef->getName(), {}, C);
    auto tryDoneRecordingExpr = TryExpr::createImplicit(C, sloc,
      CallExpr::createImplicit(C,
        UnresolvedDotExpr::createImplicit(C,
//          UnresolvedDeclRefExpr::createImplicit(C,invocationVar->getName()),
          new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc, implicit, AccessSemantics::Ordinary, invocationVar->getInterfaceType()),
          doneRecordingDecl->getName()),
        argsList));

    remoteBranchStmts.push_back(tryDoneRecordingExpr);
  }

  // === Prepare the 'RemoteCallTarget'
  VarDecl *targetVar =
      new (C) VarDecl(/*isStatic=*/false, VarDecl::Introducer::Let, sloc,
                      C.getIdentifier("target"), thunk);

  {
    // --- Mangle the thunk name
    Mangle::ASTMangler mangler;
    auto mangled = mangler.mangleEntity(thunk, swift::Mangle::ASTMangler::SymbolKind::DistributedThunk);
    StringRef mangledTargetStringRef = StringRef(mangled);
    auto mangledTargetStringLiteral = new (C)
        StringLiteralExpr(mangledTargetStringRef, SourceRange(), implicit);

    // --- let target = RemoteCallTarget(<mangled name>)
    targetVar->setInterfaceType(remoteCallTargetTy);
    targetVar->setImplicit();
    targetVar->setSynthesized();

    Pattern *targetPattern = NamedPattern::createImplicit(C, targetVar);

    auto remoteCallTargetInitDecl =
        RCT->getDistributedRemoteCallTargetInitFunction();
    auto remoteCallTargetInitDeclRef = UnresolvedDeclRefExpr::createImplicit(
        C, remoteCallTargetInitDecl->getEffectiveFullName());
//    remoteCallTargetInitDeclRef->setType(RCT->getInterfaceType());

//    auto initTargetExpr = TypeExpr::createImplicit(
//        RCT->getInterfaceType(), C);
    auto initTargetExpr = UnresolvedDeclRefExpr::createImplicit(
        C, RCT->getName());
    auto initTargetArgs = ArgumentList::forImplicitCallTo(
        remoteCallTargetInitDeclRef->getName(),
        {mangledTargetStringLiteral}, C);

    auto initTargetCallExpr =
        CallExpr::createImplicit(C, initTargetExpr, initTargetArgs);

    auto targetPB = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, targetPattern, initTargetCallExpr, thunk);

    remoteBranchStmts.push_back(targetPB);
    remoteBranchStmts.push_back(targetVar);
  }


  // === Make the 'remoteCall(Void)(...)'
  {
    auto remoteCallDecl =
        C.getRemoteCallOnDistributedActorSystem(systemDecl, isVoidReturn);
    auto systemRemoteCallRef =
        UnresolvedDotExpr::createImplicit(
            C, new (C) DeclRefExpr(systemProperty, dloc, implicit),
            remoteCallDecl->getName());

    SmallVector<Expr *, 5> args;
    // on actor: Act
    args.push_back(new (C) DeclRefExpr(selfDecl, dloc, implicit,
                                       swift::AccessSemantics::Ordinary,
                                       selfDecl->getInterfaceType()));
    // target: RemoteCallTarget
    args.push_back(new (C) DeclRefExpr(ConcreteDeclRef(targetVar), dloc, implicit,
                                       AccessSemantics::Ordinary,
                                       RCT->getDeclaredInterfaceType()));
    // invocation: inout InvocationEncoder
    args.push_back(new (C) InOutExpr(sloc,
        new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc,
        implicit, AccessSemantics::Ordinary, invocationEncoderTy), invocationEncoderTy, implicit));
    // throwing: Err.Type
    args.push_back(func->hasThrows() ? errorTypeExpr : neverTypeExpr);
    // returning: Res.Type
    if (!isVoidReturn) {
      args.push_back(resultTypeExpr);
    }

    assert(args.size() == remoteCallDecl->getParameters()->size());
    auto remoteCallArgs = ArgumentList::forImplicitCallTo(
        systemRemoteCallRef->getName(), args, C);

    Expr *remoteCallExpr =
        CallExpr::createImplicit(C, systemRemoteCallRef, remoteCallArgs);
    remoteCallExpr = AwaitExpr::createImplicit(C, sloc, remoteCallExpr);
    remoteCallExpr = TryExpr::createImplicit(C, sloc, remoteCallExpr);
    auto returnRemoteCall =
                new (C) ReturnStmt(sloc, remoteCallExpr, implicit);
    remoteBranchStmts.push_back(returnRemoteCall);
  }


  // ---------------------------------------------------------------------------
  auto remoteBranchStmt =
      BraceStmt::create(C, sloc, remoteBranchStmts, sloc, implicit);

  // ---------------------------------------------------------------------------
  // === if (isRemote(...) <remote branch> else <local branch>
  auto ifStmt = new (C) IfStmt(sloc, /*condition=*/isRemote,
                               /*then=*/remoteBranchStmt, sloc,
                               /*else=*/localBranchStmt, implicit, C);

  auto body = BraceStmt::create(C, sloc, {ifStmt}, sloc, implicit);
  return {body, /*isTypeChecked=*/false};
}

static FuncDecl *createDistributedThunkFunction(FuncDecl *func) {
  auto &C = func->getASTContext();
  auto DC = func->getDeclContext();
  auto module = func->getParentModule();
  TypeChecker::typeCheckDecl(func);

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

  std::string thunkNameString = "$dist_";
  thunkNameString.append(std::string(func->getBaseName().getIdentifier().str()));
  auto thunkBaseName = DeclBaseName(C.getIdentifier(StringRef(thunkNameString)));
  DeclName thunkName = DeclName(C, thunkBaseName, func->getParameters());

  GenericParamList *genericParamList = nullptr;
  if (auto genericParams = func->getGenericParams()) {
    genericParamList = genericParams->clone(DC);
  }
  auto funcParams = func->getParameters();
  ParameterList *params = funcParams->clone(C);
  for (auto i : indices(*func->getParameters())) {
    params->get(i)->setInterfaceType(funcParams->get(i)->getInterfaceType());
  }

    auto thunk = FuncDecl::createImplicit(C, swift::StaticSpellingKind::None,
                           thunkName, SourceLoc(),
                           /*async=*/true, /*throws=*/true,
                           genericParamList,
                           params,
                           func->getResultInterfaceType(),
                           DC);
  thunk->setSynthesized(true);
  thunk->getAttrs().add(new (C) NonisolatedAttr(/*implicit=*/true));
  thunk->setGenericSignature(func->getGenericSignature());
  thunk->copyFormalAccessFrom(func, /*sourceIsParentContext=*/false);
  thunk->setBodySynthesizer(deriveBodyDistributed_thunk, func);

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
  if (isa<ProtocolDecl>(DC)) {
    // don't synthesize thunks for protocols
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

  auto DAS = C.getProtocol(KnownProtocolKind::DistributedActorSystem);
  auto f = DAS->lookupDirect(C.Id_makeInvocationEncoder);

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
  auto DistSystemProtocol =
      C.getProtocol(KnownProtocolKind::DistributedActorSystem);

  if (auto proto = dyn_cast<ProtocolDecl>(actor)) {
    if (auto systemAssocTyDecl = proto->getAssociatedType(C.Id_ActorSystem)) {
      assert(false && "handling assoc type in protocol not handled");
    }

    auto DistributedActorProto = C.getDistributedActorDecl();
    for (auto system : DistributedActorProto->lookupDirect(C.Id_actorSystem)) {
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
