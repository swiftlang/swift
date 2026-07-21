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

#include "CodeSynthesis.h"
#include "DerivedConformance/DerivedConformance.h"
#include "TypeChecker.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/DistributedDecl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SynthesizedDeclBuilder.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

/******************************************************************************/
/*********************** DISTRIBUTED THUNK SYNTHESIS **************************/
/******************************************************************************/

static void forwardParameters(AbstractFunctionDecl *afd,
                              SmallVectorImpl<Expr*> &forwardingParams) {
  auto &C = afd->getASTContext();
  for (auto param : *afd->getParameters()) {
    forwardingParams.push_back(new (C) DeclRefExpr(
        ConcreteDeclRef(param), DeclNameLoc(), /*implicit=*/true,
        swift::AccessSemantics::Ordinary,
        afd->mapTypeIntoEnvironment(param->getInterfaceType())));
  }
}

static Type getDistributedResolvableProtocolStubType(Type ty) {
  if (!ty)
    return Type();
  auto match = findDistributedResolvableExistentialOrOpaqueProtocol(ty);
  if (!match)
    return Type();
  auto *stub = getDistributedResolvableProtocolStubDecl(match.proto);
  if (!stub)
    return Type();
  auto stubTy = stub->getDeclaredInterfaceType();
  if (!stubTy || stubTy->hasError())
    return Type();
  return stubTy;
}

/// Build `try $P.resolve(id: <idExpr>, using: <systemExpr>)`.
static Expr *createDistributedResolveCall(ASTContext &C,
                                          Type actorTy,
                                          Expr *idExpr,
                                          Expr *systemExpr) {
  auto *stubTypeExpr = TypeExpr::createImplicit(actorTy, C);
  DeclName resolveName(C, C.getIdentifier("resolve"),
                       {C.Id_id, C.getIdentifier("using")});
  auto *resolveExpr =
      UnresolvedDotExpr::createImplicit(C, stubTypeExpr, resolveName);
  auto *resolveArgList = ArgumentList::forImplicitCallTo(
      DeclNameRef(resolveName), {idExpr, systemExpr}, C);
  auto *resolveCall =
      CallExpr::createImplicit(C, resolveExpr, resolveArgList);
  return TryExpr::createImplicit(C, SourceLoc(), resolveCall);
}


/// Mangle the target thunk in a way that we can look up the appropriate record.
static llvm::StringRef
mangleDistributedThunkForAccessorRecordName(
    ASTContext &C, AbstractFunctionDecl *thunk) {
  Mangle::ASTMangler mangler(C);

  // default mangling
  auto mangled =
      C.AllocateCopy(mangler.mangleDistributedThunkRef(cast<FuncDecl>(thunk)));
  return mangled;
}

static std::pair<BraceStmt *, bool>
deriveBodyDistributed_thunk(AbstractFunctionDecl *thunk, void *context) {
  auto implicit = true;
  ASTContext &C = thunk->getASTContext();

  // mock locations, we're a thunk and don't really need detailed locations
  const SourceLoc sloc = SourceLoc();
  const DeclNameLoc dloc = DeclNameLoc();

  auto func = static_cast<FuncDecl *>(context);
  auto funcDC = func->getDeclContext();
  assert(funcDC->getSelfNominalTypeDecl() &&
         funcDC->getSelfNominalTypeDecl()->isDistributedActor() &&
         "Distributed function must be part of distributed actor");

  auto selfDecl = thunk->getImplicitSelfDecl();
  selfDecl->addAttribute(new (C) KnownToBeLocalAttr(implicit));

  // === return type
  Type returnTy = func->getResultInterfaceType();
  auto isVoidReturn = returnTy->isVoid();

  // === Type:
  StructDecl *RCT = C.getRemoteCallTargetDecl();
  assert(RCT && "Missing RemoteCalLTarget declaration");
  Type remoteCallTargetTy = RCT->getDeclaredInterfaceType();

  // === __isRemoteActor(self)
  ArgumentList *isRemoteArgs = ArgumentList::forImplicitSingle(
      C, /*label=*/Identifier(), new (C) DeclRefExpr(selfDecl, dloc, implicit));

  FuncDecl *isRemoteFn = C.getIsRemoteDistributedActor();
  assert(isRemoteFn && "Could not find 'is remote' function, is the "
                       "'_Distributed' module available?");
  auto isRemoteDeclRef =
      UnresolvedDeclRefExpr::createImplicit(C, isRemoteFn->getName());
  auto isRemote =
      CallExpr::createImplicit(C, isRemoteDeclRef, isRemoteArgs);

  // === local branch ----------------------------------------------------------
  BraceStmt *localBranchStmt;
  if (auto accessor = dyn_cast<AccessorDecl>(func)) {
    auto selfRefExpr = new (C) DeclRefExpr(selfDecl, dloc, implicit);

    auto var = accessor->getStorage();

    Expr *localPropertyAccess = new (C) MemberRefExpr(
        selfRefExpr, sloc, ConcreteDeclRef(var), dloc, implicit);
    localPropertyAccess =
        AwaitExpr::createImplicit(C, sloc, localPropertyAccess);
    if (accessor->hasThrows()) {
      localPropertyAccess =
          TryExpr::createImplicit(C, sloc, localPropertyAccess);
    }

    auto returnLocalPropertyAccess =
        ReturnStmt::createImplicit(C, sloc, localPropertyAccess);
    localBranchStmt =
        BraceStmt::create(C, sloc, {returnLocalPropertyAccess}, sloc, implicit);
  } else {
    // normal function
    auto selfRefExpr = new (C) DeclRefExpr(selfDecl, dloc, implicit);

    // -- forward arguments
    SmallVector<Expr*, 4> forwardingParams;
    forwardParameters(thunk, forwardingParams);
    auto funcRef = UnresolvedDeclRefExpr::createImplicit(C, func->getName());
    auto forwardingArgList = ArgumentList::forImplicitCallTo(funcRef->getName(), forwardingParams, C);
    auto funcDeclRef =
        UnresolvedDotExpr::createImplicit(C, selfRefExpr, func->getBaseName());

    Expr *localFuncCall = CallExpr::createImplicit(C, funcDeclRef, forwardingArgList);
    localFuncCall = AwaitExpr::createImplicit(C, sloc, localFuncCall);
    if (func->hasThrows()) {
      localFuncCall = TryExpr::createImplicit(C, sloc, localFuncCall);
    }
    auto returnLocalFuncCall =
        ReturnStmt::createImplicit(C, sloc, localFuncCall);

    localBranchStmt =
        BraceStmt::create(C, sloc, {returnLocalFuncCall}, sloc, implicit);
  }
  // === remote branch  --------------------------------------------------------
  SmallVector<ASTNode, 8> remoteBranchStmts;
  // --- self.actorSystem
  auto systemRefExpr =
      UnresolvedDotExpr::createImplicit(
          C, new (C) DeclRefExpr(selfDecl, dloc, implicit), //  TODO: make createImplicit
          C.Id_actorSystem);

  VarDecl *systemVar =
      VarDeclBuilder(thunk, C.Id_system).introducer(VarDecl::Introducer::Let);

  Pattern *systemPattern = NamedPattern::createImplicit(C, systemVar);

  auto systemPB = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, systemPattern, systemRefExpr,
      thunk);

  remoteBranchStmts.push_back(systemPB);
  remoteBranchStmts.push_back(systemVar);

  // --- invocationEncoder = system.makeInvocationEncoder()
  VarDecl *invocationVar = VarDeclBuilder(thunk, C.Id_invocation)
                               .introducer(VarDecl::Introducer::Var);

  {
    Pattern *invocationPattern = NamedPattern::createImplicit(C, invocationVar);

    auto makeInvocationExpr = UnresolvedDotExpr::createImplicit(
        C, new (C) DeclRefExpr(ConcreteDeclRef(systemVar), dloc, implicit),
        DeclName(C.Id_makeInvocationEncoder));
    auto *makeInvocationArgs = ArgumentList::createImplicit(C, {});
    auto makeInvocationCallExpr =
        CallExpr::createImplicit(C, makeInvocationExpr, makeInvocationArgs);
    makeInvocationCallExpr->setThrows(nullptr);

    auto invocationEncoderPB = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, invocationPattern, makeInvocationCallExpr,
        thunk);
    remoteBranchStmts.push_back(invocationEncoderPB);
    remoteBranchStmts.push_back(invocationVar);
  }

  // --- Recording invocation details
  // -- recordGenericSubstitution(s)
  if (auto genEnv = thunk->getGenericEnvironment()) {
    auto recordGenericSubstitutionName =
        DeclName(C, C.Id_recordGenericSubstitution,
                 /*labels=*/{Identifier()});
    auto recordGenericSubstitutionDeclRef =
        UnresolvedDeclRefExpr::createImplicit(C, recordGenericSubstitutionName);

    for (auto genParamType : genEnv->getGenericParams()) {
      auto tyExpr = TypeExpr::createImplicit(genEnv->mapTypeIntoEnvironment(genParamType), C);
      auto subTypeExpr = new (C) DotSelfExpr(
          tyExpr,
          sloc, sloc, tyExpr->getType());

      auto recordGenericSubArgsList =
          ArgumentList::forImplicitCallTo(
              recordGenericSubstitutionDeclRef->getName(),
              {subTypeExpr},
              C);

      Expr *recordGenericSub = CallExpr::createImplicit(
          C,
          UnresolvedDotExpr::createImplicit(
              C,
              new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc,
                                  implicit, AccessSemantics::Ordinary),
              recordGenericSubstitutionName),
          recordGenericSubArgsList);
      recordGenericSub = TryExpr::createImplicit(C, sloc, recordGenericSub);

      remoteBranchStmts.push_back(recordGenericSub);
    }
  }

  // -- recordArgument(s)
  {
    auto recordArgumentName = DeclName(C, C.Id_recordArgument,
                                       /*labels=*/{Identifier()});
    if (auto params = thunk->getParameters()) {
      if (params->begin())
      for (auto param : *params) {
        auto argumentName = param->getArgumentName().str();
        LiteralExpr *argumentLabelArg;
        if (argumentName.empty()) {
          argumentLabelArg = new (C) NilLiteralExpr(sloc, implicit);
        } else {
          argumentLabelArg =
              new (C) StringLiteralExpr(argumentName, SourceRange(), implicit);
        }
        auto parameterName = param->getParameterName().str();


        // --- Prepare the RemoteCallArgument<Value> for the argument
        auto argumentVarName = C.getIdentifier("_" + parameterName.str());
        StructDecl *RCA = C.getRemoteCallArgumentDecl();
        VarDecl *callArgVar = VarDeclBuilder(thunk, argumentVarName)
                                  .introducer(VarDecl::Introducer::Let);

        Pattern *callArgPattern = NamedPattern::createImplicit(C, callArgVar);

        auto remoteCallArgumentInitDecl =
            RCA->getDistributedRemoteCallArgumentInitFunction();

        // If the parameter is `some P` / `any P` (or `Optional`) where
        // P is a `@Resolvable protocol`, encode the parameter
        // as the macro-generated `$P` stub type.
        Type paramTy =
            thunk->mapTypeIntoEnvironment(param->getInterfaceType());
        Expr *argumentDeclRefExpr = new (C) DeclRefExpr(
            ConcreteDeclRef(param), dloc, implicit,
            AccessSemantics::Ordinary, paramTy);

        // --- Automatic @Resolvable protocol proxying
        //
        // If @Resolvable protocol parameter, substitute with $P.resolve()'d reference
        // Because sending a `some P` or `any P` means transferring a `$P` on the wire,
        // as the remote peer may not know our concrete P implementation, we need to send the "proxy".
        //
        // This way the remote side will decode it as `$P` proxy, which conforms to `P`,
        // so the `some/any P` parameter is correctly filled in by a `$P` instance on
        // the recipient without ever knowing the concrete type of the sender.
        if (auto resolvableMatch =
                findDistributedResolvableExistentialOrOpaqueProtocol(paramTy)) {
          if (auto *stub = getDistributedResolvableProtocolStubDecl(resolvableMatch.proto)) {
            auto stubInterfaceTy = stub->getDeclaredInterfaceType();
            if (stubInterfaceTy && !stubInterfaceTy->hasError()) {
              // Important! We replace the type of the parameter with `$P`
              paramTy = thunk->mapTypeIntoEnvironment(stubInterfaceTy);

              // --- We then have to make the parameter be actually a `$P`
              // TODO: It would be simpler if we did just create a new `$P`,
              //  but we'd need to allow `self.id = id` inside distributed actors;
              //  Once we allow that, we can just create an instance without this fake resolve.
              {
                // paramRef.id
                auto *paramIdExpr = UnresolvedDotExpr::createImplicit(
                    C, argumentDeclRefExpr, C.Id_id);

                // Get the `system` from the actor that the call is being made on.
                auto *systemRef = new (C) DeclRefExpr(
                    ConcreteDeclRef(systemVar), dloc, implicit);

                // try $P.resolve(id: paramRef.id, using: system)
                // We have enforced in sema that the system must be compatible.
                argumentDeclRefExpr = createDistributedResolveCall(
                    C, stubInterfaceTy, paramIdExpr, systemRef);
              }
            }
          }
        }

        auto boundRCAType =
            BoundGenericType::get(RCA, Type(), {paramTy});
        auto remoteCallArgumentInitDeclRef =
            TypeExpr::createImplicit(boundRCAType, C);

        auto initCallArgArgs = ArgumentList::forImplicitCallTo(
            DeclNameRef(remoteCallArgumentInitDecl->getEffectiveFullName()),
            {
             // label:
             argumentLabelArg,
             // name:
             new (C) StringLiteralExpr(parameterName, SourceRange(), implicit),
             // _ argument:
             argumentDeclRefExpr
            },
            C);

        auto initCallArgCallExpr =
            CallExpr::createImplicit(C, remoteCallArgumentInitDeclRef, initCallArgArgs);

        auto callArgPB = PatternBindingDecl::createImplicit(
            C, StaticSpellingKind::None, callArgPattern, initCallArgCallExpr, thunk);

        remoteBranchStmts.push_back(callArgPB);
        remoteBranchStmts.push_back(callArgVar);

        /// --- Pass the argumentRepr to the recordArgument function
        auto recordArgArgsList = ArgumentList::forImplicitCallTo(
            DeclNameRef(recordArgumentName),
            {new (C) DeclRefExpr(ConcreteDeclRef(callArgVar), dloc, implicit,
                                 AccessSemantics::Ordinary)},
            C);

        auto tryRecordArgExpr = TryExpr::createImplicit(
            C, sloc,
            CallExpr::createImplicit(
                C,
                UnresolvedDotExpr::createImplicit(
                    C,
                    new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc,
                                        implicit, AccessSemantics::Ordinary),
                    recordArgumentName),
                recordArgArgsList));

        remoteBranchStmts.push_back(tryRecordArgExpr);
      }
    }
  }

  // -- recordErrorType
  if (func->hasThrows()) {
    auto recordErrorTypeName = DeclName(C, C.Id_recordErrorType,
                                        /*labels=*/{Identifier()});
    // Error.self
    auto errorDecl = C.getErrorDecl();
    auto *errorTypeExpr = new (C) DotSelfExpr(
        UnresolvedDeclRefExpr::createImplicit(C, errorDecl->getName()), sloc,
        sloc, errorDecl->getDeclaredInterfaceType());

    auto recordArgsList = ArgumentList::forImplicitCallTo(
        DeclNameRef(recordErrorTypeName), {errorTypeExpr}, C);
    auto tryRecordErrorTyExpr = TryExpr::createImplicit(
        C, sloc,
        CallExpr::createImplicit(
            C,
            UnresolvedDotExpr::createImplicit(
                C,
                new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc,
                                    implicit, AccessSemantics::Ordinary),
                recordErrorTypeName),
            recordArgsList));

    remoteBranchStmts.push_back(tryRecordErrorTyExpr);
  }

  // -- recordReturnType
  if (!isVoidReturn) {
    auto recordReturnTypeName = DeclName(C, C.Id_recordReturnType,
                                         /*labels=*/{Identifier()});

    // Result.self
    // Watch out and always map into thunk context
    auto resultType = thunk->mapTypeIntoEnvironment(func->getResultInterfaceType());

    // --- `@Resolvable protocol` result: substitute `$P` for `any/some P`
    if (Type stubTy = getDistributedResolvableProtocolStubType(resultType))
      resultType = thunk->mapTypeIntoEnvironment(stubTy);

    auto *metaTypeRef = TypeExpr::createImplicit(resultType, C);
    auto *resultTypeExpr =
        new (C) DotSelfExpr(metaTypeRef, sloc, sloc, resultType);

    auto recordArgsList = ArgumentList::forImplicitCallTo(
        DeclNameRef(recordReturnTypeName), {resultTypeExpr}, C);
    auto tryRecordReturnTyExpr = TryExpr::createImplicit(
        C, sloc,
        CallExpr::createImplicit(
            C,
            UnresolvedDotExpr::createImplicit(
                C,
                new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc,
                                    implicit, AccessSemantics::Ordinary),
                recordReturnTypeName),
            recordArgsList));

    remoteBranchStmts.push_back(tryRecordReturnTyExpr);
  }

  // -- doneRecording
  {
    DeclName doneRecordingName(C.Id_doneRecording);
    auto argsList =
        ArgumentList::forImplicitCallTo(DeclNameRef(doneRecordingName), {}, C);
    auto tryDoneRecordingExpr = TryExpr::createImplicit(
        C, sloc,
        CallExpr::createImplicit(
            C,
            UnresolvedDotExpr::createImplicit(
                C,
                new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc,
                                    implicit, AccessSemantics::Ordinary,
                                    invocationVar->getInterfaceType()),
                doneRecordingName),
            argsList));

    remoteBranchStmts.push_back(tryDoneRecordingExpr);
  }

  // === Prepare the 'RemoteCallTarget'
  VarDecl *targetVar = VarDeclBuilder(thunk, C.Id_target)
                           .introducer(VarDecl::Introducer::Let)
                           .type(remoteCallTargetTy);

  {
    // --- Mangle the thunk name
    auto mangledAccessorRecordName =
        mangleDistributedThunkForAccessorRecordName(C, thunk);

    StringLiteralExpr *mangledTargetStringLiteral =
        new (C) StringLiteralExpr(mangledAccessorRecordName,
                                  SourceRange(), implicit);

    // --- let target = RemoteCallTarget(<mangled name>)
    Pattern *targetPattern = NamedPattern::createImplicit(C, targetVar);

    auto remoteCallTargetInitDecl =
        RCT->getDistributedRemoteCallTargetInitFunction();
    auto remoteCallTargetInitDeclRef = UnresolvedDeclRefExpr::createImplicit(
        C, remoteCallTargetInitDecl->getEffectiveFullName());

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
    DeclName remoteCallName;
    if (isVoidReturn) {
      remoteCallName =
          DeclName(C, C.Id_remoteCallVoid,
                   {C.Id_on, C.Id_target, C.Id_invocation, C.Id_throwing});
    } else {
      remoteCallName = DeclName(C, C.Id_remoteCall,
                                {C.Id_on, C.Id_target, C.Id_invocation,
                                 C.Id_throwing, C.Id_returning});
    }

    auto systemRemoteCallRef = UnresolvedDotExpr::createImplicit(
        C, new (C) DeclRefExpr(ConcreteDeclRef(systemVar), dloc, implicit),
        remoteCallName);

    SmallVector<Expr *, 5> args;
    // -- on actor: Act
    args.push_back(new (C) DeclRefExpr(selfDecl, dloc, implicit,
                                       swift::AccessSemantics::Ordinary,
                                       selfDecl->getInterfaceType()));
    // -- target: RemoteCallTarget
    args.push_back(new (C) DeclRefExpr(ConcreteDeclRef(targetVar), dloc, implicit,
                                       AccessSemantics::Ordinary,
                                       RCT->getDeclaredInterfaceType()));
    // -- invocation: inout InvocationEncoder
    args.push_back(new (C) InOutExpr(
        sloc,
        new (C) DeclRefExpr(ConcreteDeclRef(invocationVar), dloc, implicit,
                            AccessSemantics::Ordinary),
        Type(), implicit));

    // -- throwing: Err.Type
    if (func->hasThrows()) {
      // Error.self
      auto errorDecl = C.getErrorDecl();
      auto *errorTypeExpr = new (C) DotSelfExpr(
          UnresolvedDeclRefExpr::createImplicit(C, errorDecl->getName()), sloc,
          sloc, errorDecl->getDeclaredInterfaceType());

      args.push_back(errorTypeExpr);
    } else {
      // Never.self
      auto neverDecl = C.getNeverDecl();
      auto *neverTypeExpr = new (C) DotSelfExpr(
          UnresolvedDeclRefExpr::createImplicit(C, neverDecl->getName()), sloc,
          sloc, neverDecl->getDeclaredInterfaceType());
      args.push_back(neverTypeExpr);
    }

    // -- returning: Res.Type
    if (!isVoidReturn) {
      // Result.self
      auto resultType =
          func->mapTypeIntoEnvironment(func->getResultInterfaceType());

      // --- `@Resolvable protocol` result: substitute `$P` for `any/some P`
      if (Type stubTy = getDistributedResolvableProtocolStubType(resultType))
        resultType = func->mapTypeIntoEnvironment(stubTy);

      auto *metaTypeRef = TypeExpr::createImplicit(resultType, C);
      auto *resultTypeExpr =
          new (C) DotSelfExpr(metaTypeRef, sloc, sloc, resultType);

      args.push_back(resultTypeExpr);
    }

    assert(args.size() == (isVoidReturn ? 4 : 5));
    auto remoteCallArgs = ArgumentList::forImplicitCallTo(
        systemRemoteCallRef->getName(), args, C);

    Expr *remoteCallExpr =
        CallExpr::createImplicit(C, systemRemoteCallRef, remoteCallArgs);
    remoteCallExpr = AwaitExpr::createImplicit(C, sloc, remoteCallExpr);
    remoteCallExpr = TryExpr::createImplicit(C, sloc, remoteCallExpr);
    auto returnRemoteCall = ReturnStmt::createImplicit(C, sloc, remoteCallExpr);
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

/// Create a new FuncDecl that has the same signature as the passed in func.
/// This is used both to create stub witnesses as well as distributed thunks.
///
/// \param DC The declaration context of the newly created function
static FuncDecl *createSameSignatureDistributedThunkDecl(DeclContext *DC,
                                                         FuncDecl *func) {
  auto &C = func->getASTContext();

  // --- Prepare generic parameters
  GenericParamList *genericParamList = nullptr;
  if (auto genericParams = func->getGenericParams()) {
    genericParamList = genericParams->clone(DC);
  }

  GenericSignature baseSignature = func->getGenericSignature();

  // --- Prepare parameters
  auto funcParams = func->getParameters();
  SmallVector<ParamDecl*, 2> paramDecls;
  for (unsigned i : indices(*func->getParameters())) {
    auto funcParam = funcParams->get(i);

    auto paramName = funcParam->getParameterName();
    // If internal name is empty it could only mean either
    // `_:` or `x _: ...`, so let's auto-generate a name
    // to be used in the body of a thunk.
    if (paramName.empty()) {
      paramName = C.getIdentifier("p" + llvm::utostr(i));
    }

    auto paramDecl = new (C)
        ParamDecl(SourceLoc(),
                  /*argumentNameLoc=*/SourceLoc(), funcParam->getArgumentName(),
                  /*parameterNameLoc=*/SourceLoc(), paramName, DC);

    paramDecl->setImplicit();
    paramDecl->setSending();
    paramDecl->setSpecifier(funcParam->getSpecifier());
    paramDecl->setInterfaceType(funcParam->getInterfaceType());

    paramDecls.push_back(paramDecl);
  }
  ParameterList *params = ParameterList::create(C, paramDecls);

  FuncDecl *thunk;
  if (auto accessor = dyn_cast<AccessorDecl>(func)) {
    auto accessorThunk = AccessorDecl::createImplicit(
        C, AccessorKind::DistributedGet,
        /*storage=*/accessor->getStorage(),
        /*async=*/true, /*throws=*/true, // since it's a distributed thunk
        /*thrownType=*/TypeLoc::withoutLoc(Type()),
        func->getResultInterfaceType(),
        DC);
    accessorThunk->setParameters(params);
    // An accessor does not have a name; the `var` does though,
    // and we'll be mangling the accessor based on the Storage name (the var)
    thunk = accessorThunk;
  } else {
    // Let's use the name of a 'distributed func'
    DeclName thunkName = func->getName();

    thunk = FuncDecl::createImplicit(
        C, swift::StaticSpellingKind::None,
        thunkName, SourceLoc(),
        /*async=*/true, /*throws=*/true, // since it's a distributed thunk
        /*thrownType=*/Type(),
        genericParamList,
        params, func->getResultInterfaceType(), DC);
  }
  thunk->setSynthesized(true);

  if (isa<ClassDecl>(DC))
    thunk->addAttribute(new (C) FinalAttr(/*isImplicit=*/true));

  thunk->setGenericSignature(baseSignature);
  thunk->copyFormalAccessFrom(func, /*sourceIsParentContext=*/false);

  thunk->setSynthesized(true);
  thunk->setDistributedThunk(true);
  thunk->addAttribute(NonisolatedAttr::createImplicit(C));
  // TODO(distributed): It would be nicer to make distributed thunks nonisolated(nonsending) instead;
  //                    this way we would not hop off the caller when calling system.remoteCall;
  //                    it'd need new ABI and the remoteCall also to become nonisolated(nonsending)
  thunk->addAttribute(new (C) ConcurrentAttr(/*IsImplicit=*/true));

  return thunk;
}

static FuncDecl *createDistributedThunkFunction(FuncDecl *func) {
  auto DC = func->getDeclContext();

  FuncDecl *thunk =
      createSameSignatureDistributedThunkDecl(DC, func);
  assert(thunk && "couldn't create a distributed thunk");

  // Protocol requirements don't have bodies.
  if (func->hasBody())
    thunk->setBodySynthesizer(deriveBodyDistributed_thunk, func);

  return thunk;
}

// ==== ----------------------------------------------------------------------
// MARK: 'resolvable proxy adapter' thunk synthesis

/// Synthesize the body of the 'resolvable proxy adapter' thunk.
///
/// This thunk runs on the *recipient* side of a remote call. Its parameters
/// and result are the wire-level proxy stub types (`$P`), whereas the
/// user-declared distributed function deals in `any P` / `some P`. This thunk:
///
///   1. forwards the proxy parameters to the user function -- a `$P` is
///      implicitly erased to `any P` (it conforms to `P`), or bound to a
///      `some P` generic parameter directly;
///   2. invokes the user-declared distributed function on the local actor;
///   3. if the result is a `@Resolvable` `any P` / `some P`, re-creates a
///      proxy `$P` from the returned actor's identity, so a `$P` can be
///      returned over the wire.
///
/// For example, given `distributed func echo(_ g: any P) -> any P`, the
/// synthesized body is roughly:
///
///   nonisolated func echo(_ g: $P) async throws -> $P {
///     let __result = try await self.echo(g)
///     return try $P.resolve(id: __result.id, using: self.actorSystem)
///   }
static std::pair<BraceStmt *, bool>
deriveBodyDistributed_resolvableProxyAdapterThunk(AbstractFunctionDecl *thunk,
                                                  void *context) {
  auto implicit = true;
  ASTContext &C = thunk->getASTContext();

  const SourceLoc sloc = SourceLoc();
  const DeclNameLoc dloc = DeclNameLoc();

  auto func = static_cast<FuncDecl *>(context);
  auto funcDC = func->getDeclContext();
  assert(funcDC->getSelfNominalTypeDecl() &&
         funcDC->getSelfNominalTypeDecl()->isDistributedActor() &&
         "Distributed function must be part of distributed actor");

  auto selfDecl = thunk->getImplicitSelfDecl();
  selfDecl->addAttribute(new (C) KnownToBeLocalAttr(implicit));

  Type returnTy = func->getResultInterfaceType();
  auto isVoidReturn = returnTy->isVoid();

  // --- Build the call to the user-declared distributed function:
  //       try await self.<func>(<forwarded args>)
  //
  // The adapter thunk's `@Resolvable` parameters are typed as the proxy
  // `$P`. Passing a `$P` where the function expects `any P` is an implicit
  // existential erasure (`$P` conforms to `P`); passing it where the
  // function expects `some P` binds that generic parameter to the concrete
  // `$P`. Either way the forward is well-typed.
  Expr *call;
  {
    auto selfRefExpr = new (C) DeclRefExpr(selfDecl, dloc, implicit);

    if (auto accessor = dyn_cast<AccessorDecl>(func)) {
      auto var = accessor->getStorage();
      Expr *localPropertyAccess = new (C) MemberRefExpr(
          selfRefExpr, sloc, ConcreteDeclRef(var), dloc, implicit);
      localPropertyAccess =
          AwaitExpr::createImplicit(C, sloc, localPropertyAccess);
      if (accessor->hasThrows())
        localPropertyAccess =
            TryExpr::createImplicit(C, sloc, localPropertyAccess);
      call = localPropertyAccess;
    } else {
      SmallVector<Expr *, 4> forwardingParams;
      forwardParameters(thunk, forwardingParams);
      auto funcRef = UnresolvedDeclRefExpr::createImplicit(C, func->getName());
      auto forwardingArgList = ArgumentList::forImplicitCallTo(
          funcRef->getName(), forwardingParams, C);
      auto funcDeclRef =
          UnresolvedDotExpr::createImplicit(C, selfRefExpr, func->getBaseName());

      Expr *localFuncCall =
          CallExpr::createImplicit(C, funcDeclRef, forwardingArgList);
      localFuncCall = AwaitExpr::createImplicit(C, sloc, localFuncCall);
      if (func->hasThrows())
        localFuncCall = TryExpr::createImplicit(C, sloc, localFuncCall);
      call = localFuncCall;
    }
  }

  // --- Does the result need to be adapted back to a proxy `$P`?
  Type resultType =
      thunk->mapTypeIntoEnvironment(func->getResultInterfaceType());
  Type proxyResultTy =
      isVoidReturn ? Type() : getDistributedResolvableProtocolStubType(resultType);

  SmallVector<ASTNode, 4> stmts;
  if (!proxyResultTy) {
    // No proxying of the result needed - return the call result directly.
    auto returnCall = ReturnStmt::createImplicit(C, sloc, call);
    stmts.push_back(returnCall);
  } else {
    // let __result = try await self.<func>(...)
    // The thunk body is entirely synthesized; there are no user-written locals
    // in scope, so the `__result` name cannot collide.
    VarDecl *resultVar = VarDeclBuilder(thunk, C.getIdentifier("__result"))
                             .introducer(VarDecl::Introducer::Let);

    Pattern *resultPattern = NamedPattern::createImplicit(C, resultVar);
    auto resultPB = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, resultPattern, call, thunk);

    stmts.push_back(resultPB);
    stmts.push_back(resultVar);

    // return try $P.resolve(id: __result.id, using: self.actorSystem)
    auto *resultRef =
        new (C) DeclRefExpr(ConcreteDeclRef(resultVar), dloc, implicit);
    auto *resultIdExpr =
        UnresolvedDotExpr::createImplicit(C, resultRef, C.Id_id);

    auto *systemRef = UnresolvedDotExpr::createImplicit(
        C, new (C) DeclRefExpr(selfDecl, dloc, implicit), C.Id_actorSystem);

    Expr *resolveCall = createDistributedResolveCall(
        C, proxyResultTy, resultIdExpr, systemRef);

    auto returnResolve = ReturnStmt::createImplicit(C, sloc, resolveCall);
    stmts.push_back(returnResolve);
  }

  auto body = BraceStmt::create(C, sloc, stmts, sloc, implicit);
  return {body, /*isTypeChecked=*/false};
}

/// Create the 'resolvable proxy adapter' thunk. Its signature matches the
/// user-declared function, except that `@Resolvable` parameters (`any P`
/// or `some P`) and a `@Resolvable` result are replaced by the proxy stub
/// type `$P`.
static FuncDecl *
createDistributedResolvableProxyAdapterThunkDecl(DeclContext *DC,
                                                 FuncDecl *func) {
  auto &C = func->getASTContext();

  // --- Prepare generic parameters.
  //
  // A `some P` parameter rewritten to a concrete `$P` below leaves its
  // generic parameter unused in the thunk's signature; that is intentional
  // and harmless.
  // TODO(distributed): drop the unused generic parameter from the thunk's
  //   signature when its only use was substituted away by the `$P` rewrite.
  GenericParamList *genericParamList = nullptr;
  if (auto genericParams = func->getGenericParams())
    genericParamList = genericParams->clone(DC);

  GenericSignature baseSignature = func->getGenericSignature();

  // --- Prepare parameters
  auto funcParams = func->getParameters();
  SmallVector<ParamDecl *, 2> paramDecls;
  for (unsigned i : indices(*func->getParameters())) {
    auto funcParam = funcParams->get(i);

    auto paramName = funcParam->getParameterName();
    if (paramName.empty())
      paramName = C.getIdentifier("p" + llvm::utostr(i));

    auto paramDecl = new (C)
        ParamDecl(SourceLoc(),
                  /*argumentNameLoc=*/SourceLoc(), funcParam->getArgumentName(),
                  /*parameterNameLoc=*/SourceLoc(), paramName, DC);
    paramDecl->setImplicit();
    paramDecl->setSending();
    paramDecl->setSpecifier(funcParam->getSpecifier());

    // If the parameter is a `@Resolvable` `any P` (existential) or `some P`
    // (opaque / generic) parameter, the wire-level type is the proxy stub
    // `$P`; rewrite the parameter to `$P` in either case.
    Type paramTy = funcParam->getInterfaceType();
    Type mappedParamTy = func->mapTypeIntoEnvironment(paramTy);
    if (Type proxyTy = getDistributedResolvableProtocolStubType(mappedParamTy))
      paramTy = proxyTy;
    paramDecl->setInterfaceType(paramTy);

    paramDecls.push_back(paramDecl);
  }
  ParameterList *params = ParameterList::create(C, paramDecls);

  // --- Prepare the result type, adapting a `@Resolvable` result to `$P`.
  Type resultTy = func->getResultInterfaceType();
  if (Type proxyTy = getDistributedResolvableProtocolStubType(
          func->mapTypeIntoEnvironment(resultTy)))
    resultTy = proxyTy;

  // Synthesize a distinct, stable name so this helper neither collides with
  // the regular distributed thunk nor goes through the distributed-thunk
  // mangling path. The `$` prefix and infix make it unspellable from
  // user code: `$distributedProxyAdapter$<base>`. For a computed property
  // the base is the storage (var) name.
  Identifier baseIdent;
  if (auto *accessor = dyn_cast<AccessorDecl>(func))
    baseIdent = accessor->getStorage()->getBaseIdentifier();
  else
    baseIdent = func->getBaseIdentifier();

  SmallString<64> nameBuf;
  nameBuf += "$distributedProxyAdapter$";
  nameBuf += baseIdent.str();
  DeclName thunkName(C, C.getIdentifier(nameBuf),
                     func->getName().getArgumentNames());

  auto *thunk = FuncDecl::createImplicit(
      C, swift::StaticSpellingKind::None, thunkName, SourceLoc(),
      /*async=*/true, /*throws=*/true,
      /*thrownType=*/Type(), genericParamList, params, resultTy, DC);
  thunk->setSynthesized(true);

  if (isa<ClassDecl>(DC))
    thunk->addAttribute(new (C) FinalAttr(/*isImplicit=*/true));

  thunk->setGenericSignature(baseSignature);
  thunk->copyFormalAccessFrom(func, /*sourceIsParentContext=*/false);
  // TODO(distributed): This thunk should be `nonisolated(nonsending)` so it
  //   runs on the caller's (accessor's) executor, so we avoid an extra hop.
  //   That makes it `@caller_isolated` (an implicit leading
  //   `Builtin.ImplicitActor` parameter), which the hand-built distributed
  //   target accessor in GenDistributed.cpp does not yet pass. Until the
  //   accessor learns that ABI, match the regular distributed thunk's
  //   isolation (`nonisolated` + `@concurrent`).
  thunk->addAttribute(NonisolatedAttr::createImplicit(C));
  thunk->addAttribute(new (C) ConcurrentAttr(/*IsImplicit=*/true));

  return thunk;
}

static FuncDecl *
createDistributedResolvableProxyAdapterThunkFunction(FuncDecl *func) {
  auto DC = func->getDeclContext();

  FuncDecl *thunk = createDistributedResolvableProxyAdapterThunkDecl(DC, func);
  assert(thunk &&
         "couldn't create a distributed resolvable proxy adapter thunk");

  if (func->hasBody())
    thunk->setBodySynthesizer(
        deriveBodyDistributed_resolvableProxyAdapterThunk, func);

  return thunk;
}

/// Determine whether \p func requires a 'resolvable proxy adapter' thunk:
/// it does when it has a `@Resolvable` parameter (`any P` or `some P`) or
/// a `@Resolvable` result, all of which are rewritten to the proxy stub
/// type `$P`.
static bool
distributedTargetNeedsResolvableProxyAdapterThunk(AbstractFunctionDecl *func) {
  // Does the result need a type substitution?
  if (auto *fn = dyn_cast<FuncDecl>(func)) {
    auto resultTy = fn->mapTypeIntoEnvironment(fn->getResultInterfaceType());
    if (getDistributedResolvableProtocolStubType(resultTy))
      return true;
  }

  // Do any of the parameters need type substitution?
  for (auto *param : *func->getParameters()) {
    auto paramTy = func->mapTypeIntoEnvironment(param->getInterfaceType());
    if (getDistributedResolvableProtocolStubType(paramTy))
      return true;
  }

  return false;
}

/******************************************************************************/
/*********************** CODABLE CONFORMANCE **********************************/
/******************************************************************************/

static NormalProtocolConformance*
addDistributedActorCodableConformance(
    ClassDecl *actor, ProtocolDecl *proto) {
  assert(proto->isSpecificProtocol(swift::KnownProtocolKind::Decodable) ||
         proto->isSpecificProtocol(swift::KnownProtocolKind::Encodable));
  auto &C = actor->getASTContext();

  // === Only Distributed actors can gain this implicit conformance
  if (!actor->isDistributedActor()) {
    return nullptr;
  }

  // === Does the actor explicitly conform to the protocol already?
  auto explicitConformance =
      lookupConformance(actor->getInterfaceType(), proto);
  if (!explicitConformance.isInvalid()) {
    // ok, it was conformed explicitly -- let's not synthesize;
    return nullptr;
  }

  // Check whether we can infer conformance at all.
  if (auto *file = dyn_cast<FileUnit>(actor->getModuleScopeContext())) {
    switch (file->getKind()) {
    case FileUnitKind::Source:
      // Check what kind of source file we have.
      if (auto sourceFile = actor->getParentSourceFile()) {
        switch (sourceFile->Kind) {
        case SourceFileKind::Interface:
          return nullptr;

        case SourceFileKind::Library:
        case SourceFileKind::Main:
        case SourceFileKind::MacroExpansion:
        case SourceFileKind::SIL:
        case SourceFileKind::DefaultArgument:
        case SourceFileKind::SyntheticMacro:
          break;
        }
      }
      break;

    case FileUnitKind::Builtin:
    case FileUnitKind::SerializedAST:
    case FileUnitKind::Synthesized:
      // Explicitly-handled modules don't infer Sendable conformances.
      return nullptr;

    case FileUnitKind::ClangModule:
    case FileUnitKind::DWARFModule:
      // Infer conformances for imported modules.
      break;
    }
  } else {
    return nullptr;
  }

  auto conformance = C.getNormalConformance(
      actor->getDeclaredInterfaceType(), proto, actor->getLoc(),
      /*inheritedTypeRepr=*/nullptr, /*dc=*/actor,
      ProtocolConformanceState::Incomplete, ProtocolConformanceOptions());
  conformance->setSourceKindAndImplyingConformance(
      ConformanceEntryKind::Synthesized, nullptr);
  actor->registerProtocolConformance(conformance, /*synthesized=*/true);
  return conformance;
}

/******************************************************************************/
/******************************************************************************/

void swift::assertRequiredSynthesizedPropertyOrder(ASTContext &Context,
                                                   NominalTypeDecl *nominal) {
#ifndef NDEBUG
  if (nominal->getDistributedActorIDProperty()) {
    if (nominal->getDistributedActorSystemProperty()) {
      if (auto classDecl = dyn_cast<ClassDecl>(nominal)) {
        if (classDecl->getUnownedExecutorProperty()) {
          int idIdx, actorSystemIdx, unownedExecutorIdx = 0;
          int idx = 0;
          for (auto member : nominal->getMembers()) {
            if (auto binding = dyn_cast<PatternBindingDecl>(member)) {
              if (binding->getSingleVar()->getName() == Context.Id_id) {
                idIdx = idx;
              } else if (binding->getSingleVar()->getName() ==
                         Context.Id_actorSystem) {
                actorSystemIdx = idx;
              } else if (binding->getSingleVar()->getName() ==
                         Context.Id_unownedExecutor) {
                unownedExecutorIdx = idx;
              }
              idx += 1;
            }
          }
          if (idIdx + actorSystemIdx + unownedExecutorIdx >= 0 + 1 + 2) {
            // we have found all the necessary fields, let's assert their order
            // FIXME: This assertion was not asserting what it is designed to
            // assert and more work is needed to make it pass.
//            assert(idIdx < actorSystemIdx < unownedExecutorIdx &&
//                   "order of fields MUST be exact.");
          }
        }
      }
    }
  }
#endif
}

static bool canSynthesizeDistributedThunk(AbstractFunctionDecl *distributedTarget) {
  // `distributed` protocol requirements are allowed without additional checks.
  if (isa<ProtocolDecl>(distributedTarget->getDeclContext()))
    return true;

  if (getConcreteReplacementForProtocolActorSystemType(distributedTarget)) {
    return true;
  }

  auto serializationTy =
      getDistributedActorSerializationType(distributedTarget->getDeclContext());
  return !serializationTy->hasError() && !serializationTy->isTypeParameter();
}

/******************************************************************************/
/*********************** SYNTHESIS ENTRY POINTS *******************************/
/******************************************************************************/

FuncDecl *GetDistributedThunkRequest::evaluate(Evaluator &evaluator,
                                               Originator originator) const {
  AbstractFunctionDecl *distributedTarget = nullptr;
  if (auto *storage = originator.dyn_cast<AbstractStorageDecl *>()) {
    if (!storage->isDistributed())
      return nullptr;

    if (auto *var = dyn_cast<VarDecl>(storage)) {
      if (checkDistributedActorProperty(var, /*diagnose=*/false))
        return nullptr;

      distributedTarget = var->getAccessor(AccessorKind::Get);
    } else {
      llvm_unreachable("unsupported storage kind");
    }
  } else {
    distributedTarget = cast<AbstractFunctionDecl *>(originator);
    if (!distributedTarget->isDistributed())
      return nullptr;
  }
  assert(distributedTarget);

  // This evaluation type-check by now was already computed and cached;
  // We need to check in order to avoid emitting a THUNK for a distributed func
  // which had errors; as the thunk then may also cause un-addressable issues and confusion.
  if (swift::checkDistributedFunction(distributedTarget)) {
    return nullptr;
  }

  auto &C = distributedTarget->getASTContext();

  if (!canSynthesizeDistributedThunk(distributedTarget)) {
    return nullptr;
  }

  // If the target function signature has errors, or if it is illegal in other
  // ways, such as e.g. parameters not conforming to SerializationRequirement,
  // we must avoid synthesis of the thunk because it'd also have errors,
  // giving an ugly user experience (errors in implicit code).
  if (distributedTarget->getInterfaceType()->hasError() ||
      (!isa<AccessorDecl>(distributedTarget) &&
       checkDistributedFunction(distributedTarget))) {
    return nullptr;
  }

  if (auto func = dyn_cast<FuncDecl>(distributedTarget)) {
    // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
    // we won't be emitting the offending decl after all.
    if (!C.getLoadedModule(C.Id_Distributed))
      return nullptr;

    // --- Prepare the "distributed thunk" which does the "maybe remote" dance:
    return createDistributedThunkFunction(func);
  }

  llvm_unreachable("Unable to synthesize distributed thunk");
}

FuncDecl *
GetDistributedRecipientResolvableProxyAdapterThunkRequest::evaluate(
    Evaluator &evaluator, Originator originator) const {
  AbstractFunctionDecl *distributedTarget = nullptr;
  if (auto *storage = originator.dyn_cast<AbstractStorageDecl *>()) {
    if (!storage->isDistributed())
      return nullptr;

    if (auto *var = dyn_cast<VarDecl>(storage)) {
      if (checkDistributedActorProperty(var, /*diagnose=*/false))
        return nullptr;

      distributedTarget = var->getAccessor(AccessorKind::Get);
    } else {
      llvm_unreachable("unsupported storage kind");
    }
  } else {
    distributedTarget = cast<AbstractFunctionDecl *>(originator);
    if (!distributedTarget->isDistributed())
      return nullptr;
  }
  assert(distributedTarget);

  // Avoid synthesizing for a target which had errors; mirrors the logic in
  // GetDistributedThunkRequest so we never emit a thunk for an invalid decl.
  if (swift::checkDistributedFunction(distributedTarget)) {
    return nullptr;
  }

  auto &C = distributedTarget->getASTContext();

  if (!canSynthesizeDistributedThunk(distributedTarget)) {
    return nullptr;
  }

  if (distributedTarget->getInterfaceType()->hasError() ||
      (!isa<AccessorDecl>(distributedTarget) &&
       checkDistributedFunction(distributedTarget))) {
    return nullptr;
  }

  if (auto func = dyn_cast<FuncDecl>(distributedTarget)) {
    // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
    // we won't be emitting the offending decl after all.
    if (!C.getLoadedModule(C.Id_Distributed))
      return nullptr;

    // The resolvable proxy adapter thunk is only needed when a parameter or
    // the result is `@Resolvable` `any P` / `some P` and so must be adapted
    // to / from the proxy stub `$P`. Otherwise, the call is made directly.
    if (!distributedTargetNeedsResolvableProxyAdapterThunk(func))
      return nullptr;

    return createDistributedResolvableProxyAdapterThunkFunction(func);
  }

  llvm_unreachable(
      "Unable to synthesize distributed resolvable proxy adapter thunk");
}

static VarDecl *lookupDistributedActorProperty(NominalTypeDecl *decl,
                                               DeclName name) {
  VarDecl *result = nullptr;
  for (auto *ref : decl->lookupDirect(name)) {
    auto *prop = dyn_cast<VarDecl>(ref);
    if (!prop || prop->getDeclContext() != decl)
      continue;

    if (!result) {
      result = prop;
      continue;
    }
    return nullptr;
  }
  return result;
}

VarDecl *
GetDistributedActorIDPropertyRequest::evaluate(Evaluator &evaluator,
                                               NominalTypeDecl *nominal) const {
  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  auto &C = nominal->getASTContext();
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  if (!isa<ClassDecl>(nominal) || !nominal->isDistributedActor())
    return nullptr;

  // If we're in a deserialized module or swift interface we expect to be able
  // to find this through name lookup.
  auto *DC = nominal->getDeclContext();
  if (!DC->getParentSourceFile() || DC->isInSwiftinterface())
    return lookupDistributedActorProperty(nominal, C.Id_id);

  // ==== Synthesize and add 'id' property to the actor decl
  VarDecl *propDecl =
      VarDeclBuilder(nominal, C.Id_id).introducer(VarDecl::Introducer::Let);
  propDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  // NOTE: The type for this property is lazily computed by
  // `getLazilySynthesizedPattern` when type-checking, which ensures this
  // request does not trigger any semantic requests since it's called by name
  // lookup.
  Pattern *propPat = NamedPattern::createImplicit(C, propDecl);

  PatternBindingDecl *pbDecl = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, propPat, /*InitExpr*/ nullptr, nominal);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->addAttribute(NonisolatedAttr::createImplicit(C));
  // mark as @_compilerInitialized, since we synthesize the initializing
  // assignment during SILGen.
  propDecl->addAttribute(new (C) CompilerInitializedAttr(/*IsImplicit=*/true));

  // IMPORTANT: The `id` MUST be the first field of any distributed actor,
  // because when we allocate remote proxy instances, we don't allocate memory
  // for anything except the first two fields: id and actorSystem, so they
  // MUST be those fields.
  //
  // Their specific order also matters, because it is enforced this way in IRGen
  // and how we emit them in AST MUST match what IRGen expects or cross-module
  // things could be using wrong offsets and manifest as reading trash memory on
  // id or system accesses.
  nominal->addMember(propDecl, /*hint=*/nullptr, /*insertAtHead=*/true);
  nominal->addMember(pbDecl, /*hint=*/nullptr, /*insertAtHead=*/true);
  return propDecl;
}

VarDecl *GetDistributedActorSystemPropertyRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  auto &C = nominal->getASTContext();
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  if (!isa<ClassDecl>(nominal) || !nominal->isDistributedActor())
    return nullptr;

  // If we're in a deserialized module or swift interface we expect to be able
  // to find this through name lookup.
  auto *DC = nominal->getDeclContext();
  if (!DC->getParentSourceFile() || DC->isInSwiftinterface())
    return lookupDistributedActorProperty(nominal, C.Id_actorSystem);

  // ==== Synthesize and add 'actorSystem' property to the actor decl
  VarDecl *propDecl = VarDeclBuilder(nominal, C.Id_actorSystem)
                          .introducer(VarDecl::Introducer::Let);
  propDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  // NOTE: The type for this property is lazily computed by
  // `getLazilySynthesizedPattern` when type-checking, which ensures this
  // request does not trigger any semantic requests since it's called by name
  // lookup.
  Pattern *propPat = NamedPattern::createImplicit(C, propDecl);

  PatternBindingDecl *pbDecl = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, propPat, /*InitExpr*/ nullptr, nominal);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->addAttribute(NonisolatedAttr::createImplicit(C));

  auto idProperty = nominal->getDistributedActorIDProperty();
  // If the id was not yet synthesized, we need to ensure that eventually
  // the order of fields will be: id, actorSystem (because IRGen needs the
  // layouts to match with the AST we produce). We do this by inserting FIRST,
  // and then as the ID gets synthesized, it'll also force FIRST and therefore
  // the order will be okey -- ID and then system.
  auto insertAtHead = idProperty == nullptr;

  // IMPORTANT: The `id` MUST be the first field of any distributed actor.
  // So we find the property and add the system AFTER it using the hint.
  //
  // If the `id` was not synthesized yet, we'll end up inserting at head,
  // but the id synthesis will force itself to be FIRST anyway, so it works out.
  nominal->addMember(propDecl, /*hint=*/idProperty, insertAtHead);
  nominal->addMember(pbDecl, /*hint=*/idProperty, insertAtHead);
  return propDecl;
}

NormalProtocolConformance *GetDistributedActorImplicitCodableRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal,
    KnownProtocolKind protoKind) const {
  assert(nominal->isDistributedActor());
  assert(protoKind == KnownProtocolKind::Encodable ||
         protoKind == KnownProtocolKind::Decodable);
  auto &C = nominal->getASTContext();

  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if (!classDecl) {
    // we only synthesize the conformance for concrete actors
    return nullptr;
  }

  return addDistributedActorCodableConformance(classDecl,
                                               C.getProtocol(protoKind));
}

bool CanSynthesizeDistributedActorCodableConformanceRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *actor) const {

  if (actor && !isa<ClassDecl>(actor))
    return false;

  if (!actor->isDistributedActor())
    return false;

  auto systemTy = getConcreteReplacementForProtocolActorSystemType(actor);
  if (!systemTy)
    return false;

  if (!systemTy->getAnyNominal())
    return false;

  auto idTy = getDistributedActorSystemActorIDType(systemTy->getAnyNominal());
  if (!idTy)
    return false;

  return TypeChecker::conformsToKnownProtocol(
             idTy, KnownProtocolKind::Decodable) &&
         TypeChecker::conformsToKnownProtocol(
             idTy, KnownProtocolKind::Encodable);
}

NormalProtocolConformance *
GetDistributedActorAsActorConformanceRequest::evaluate(
    Evaluator &evaluator, ProtocolDecl *distributedActorProto) const {
  auto &ctx = distributedActorProto->getASTContext();
  auto actorProto = ctx.getProtocol(KnownProtocolKind::Actor);

  auto ext = findDistributedActorAsActorExtension(
      distributedActorProto);
  if (!ext)
    return nullptr;

  auto distributedActorAsActorConformance = ctx.getNormalConformance(
      Type(ctx.TheSelfType), actorProto, SourceLoc(),
      /*inheritedTypeRepr=*/nullptr, ext, ProtocolConformanceState::Incomplete,
      ProtocolConformanceOptions());
  // NOTE: Normally we "register" a conformance, but here we don't
  // because we cannot (currently) register them in a protocol,
  // since they do not have conformance tables.

  return distributedActorAsActorConformance;
}
