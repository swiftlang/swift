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
#include "TypeCheckDistributed.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/DistributedDecl.h"

using namespace swift;

bool DerivedConformance::canDeriveIdentifiable(
    NominalTypeDecl *nominal, DeclContext *dc) {
  // we only synthesize for concrete 'distributed actor' decls (which are class)
  if (!isa<ClassDecl>(nominal))
    return false;

  auto &C = nominal->getASTContext();
  if (!C.getLoadedModule(C.Id_Distributed))
    return false;

  return nominal->isDistributedActor();
}

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto &C = nominal->getASTContext();
  auto classDecl = dyn_cast<ClassDecl>(nominal);

  return C.getLoadedModule(C.Id_Distributed) &&
         classDecl && classDecl->isDistributedActor() &&
         dc == nominal;
}

bool DerivedConformance::canDeriveDistributedActorSystem(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto &C = nominal->getASTContext();
  return C.getLoadedModule(C.Id_Distributed);
}

/******************************************************************************/
/******************************* RESOLVE FUNCTION *****************************/
/******************************************************************************/

/// Synthesizes the
///
/// \verbatim
/// static resolve(_ address: ActorAddress,
///                using system: DistributedActorSystem) throws -> Self {
///   <filled in by SILGenDistributed>
/// }
/// \endverbatim
///
/// factory function in the AST, with an empty body. Its body is
/// expected to be filled-in during SILGen.
static FuncDecl *deriveDistributedActor_resolve(DerivedConformance &derived) {
  auto decl = dyn_cast<ClassDecl>(derived.Nominal);
  assert(decl->isDistributedActor());
  auto &C = decl->getASTContext();

  auto idType = getDistributedActorIDType(decl);
  auto actorSystemType = getDistributedActorSystemType(decl);

  // (id: Self.ID, using system: Self.ActorSystem)
  auto *params = ParameterList::create(
      C,
      /*LParenLoc=*/SourceLoc(),
      /*params=*/{
          ParamDecl::createImplicit(
              C, C.Id_id, C.Id_id, idType, decl),
          ParamDecl::createImplicit(
              C, C.Id_using, C.Id_system, actorSystemType, decl)
      },
      /*RParenLoc=*/SourceLoc()
  );

  // Func name: resolve(id:using:)
  DeclName name(C, C.Id_resolve, params);

  // Expected type: (Self) -> (Self.ID, Self.ActorSystem) throws -> (Self)
  auto *factoryDecl =
      FuncDecl::createImplicit(C, StaticSpellingKind::KeywordStatic,
                               name, SourceLoc(),
                               /*async=*/false,
                               /*throws=*/true,
                               /*genericParams=*/nullptr,
                               params,
                               /*returnType*/decl->getDeclaredInterfaceType(),
                               decl);

  factoryDecl->setDistributedActorFactory(); // TODO(distributed): should we mark this specifically as the resolve factory?
  factoryDecl->copyFormalAccessFrom(decl, /*sourceIsParentContext=*/true);

  derived.addMembersToConformanceContext({factoryDecl});
  return factoryDecl;
}

/******************************************************************************/
/*************** INVOKE HANDLER ON-RETURN FUNCTION ****************************/
/******************************************************************************/

namespace {
struct DoInvokeOnReturnContext {
  ParamDecl *handlerParam;
  ParamDecl *resultBufferParam;
};
} // namespace

static std::pair<BraceStmt *, bool>
deriveBodyDistributed_doInvokeOnReturn(AbstractFunctionDecl *afd, void *arg) {
  auto &C = afd->getASTContext();
  auto *context = static_cast<DoInvokeOnReturnContext *>(arg);

  // mock locations, we're a thunk and don't really need detailed locations
  const SourceLoc sloc = SourceLoc();
  const DeclNameLoc dloc = DeclNameLoc();
  bool implicit = true;

  auto returnTypeParam = afd->getParameters()->get(0);
  SmallVector<ASTNode, 8> stmts;

  VarDecl *resultVar =
      new (C) VarDecl(/*isStatic=*/false, VarDecl::Introducer::Let, sloc,
                      C.getIdentifier("result"), afd);
  {
    auto resultLoadCall = CallExpr::createImplicit(
        C,
        UnresolvedDotExpr::createImplicit(
            C,
            /*base=*/
            new (C) DeclRefExpr(ConcreteDeclRef(context->resultBufferParam),
                                dloc, implicit),
            /*baseName=*/DeclBaseName(C.getIdentifier("load")),
            /*argLabels=*/
            {C.getIdentifier("fromByteOffset"), C.getIdentifier("as")}),
        ArgumentList::createImplicit(
            C, {Argument(sloc, C.getIdentifier("as"),
                         new (C) DeclRefExpr(ConcreteDeclRef(returnTypeParam),
                                             dloc, implicit))}));

    auto resultPattern = NamedPattern::createImplicit(C, resultVar);
    auto resultPB = PatternBindingDecl::createImplicit(
        C, swift::StaticSpellingKind::None, resultPattern,
        /*expr=*/resultLoadCall, afd);

    stmts.push_back(resultPB);
    stmts.push_back(resultVar);
  }

  // call the ad-hoc `handler.onReturn`
  {
    // Find the ad-hoc requirement ensured function on the concrete handler:
    auto onReturnFunc = C.getOnReturnOnDistributedTargetInvocationResultHandler(
        context->handlerParam->getInterfaceType()->getAnyNominal());
    assert(onReturnFunc && "did not find ad-hoc requirement witness!");

    Expr *callExpr = CallExpr::createImplicit(
        C,
        UnresolvedDotExpr::createImplicit(
            C,
            /*base=*/
            new (C) DeclRefExpr(ConcreteDeclRef(context->handlerParam), dloc,
                                implicit),
            /*baseName=*/onReturnFunc->getBaseName(),
            /*paramList=*/onReturnFunc->getParameters()),
        ArgumentList::forImplicitCallTo(
            DeclNameRef(onReturnFunc->getName()),
            {new (C) DeclRefExpr(ConcreteDeclRef(resultVar), dloc, implicit)},
            C));
    callExpr = TryExpr::createImplicit(C, sloc, callExpr);
    callExpr = AwaitExpr::createImplicit(C, sloc, callExpr);

    stmts.push_back(callExpr);
  }

  auto body = BraceStmt::create(C, sloc, {stmts}, sloc, implicit);
  return {body, /*isTypeChecked=*/false};
}

// Create local function:
//    func invokeOnReturn<R: Self.SerializationRequirement>(
//        _ returnType: R.Type
//    ) async throws {
//      let value = resultBuffer.load(as: returnType)
//      try await handler.onReturn(value: value)
//    }
static FuncDecl* createLocalFunc_doInvokeOnReturn(
    ASTContext& C, FuncDecl* parentFunc,
    NominalTypeDecl* systemNominal,
    ParamDecl* handlerParam,
    ParamDecl* resultBufParam) {
  auto DC = parentFunc;
  auto DAS = C.getDistributedActorSystemDecl();
  auto doInvokeLocalFuncIdent = C.getIdentifier("doInvokeOnReturn");

  // mock locations, we're a synthesized func and don't need real locations
  const SourceLoc sloc = SourceLoc();

  // <R: Self.SerializationRequirement>
  // We create the generic param at invalid depth, which means it'll be filled
  // by semantic analysis.
  auto resultGenericParamDecl = GenericTypeParamDecl::create(
      parentFunc, C.getIdentifier("R"), sloc, /*isTypeSequence=*/false,
      /*depth=*/0, /*index=*/0,
      /*isOpaqueType=*/false,
      /*opaqueTypeRepr=*/nullptr);
  GenericParamList *doInvokeGenericParamList =
      GenericParamList::create(C, sloc, {resultGenericParamDecl}, sloc);

  auto returnTypeIdent = C.getIdentifier("returnType");
  auto resultTyParamDecl =
      ParamDecl::createImplicit(C,
                                /*argument=*/returnTypeIdent,
                                /*parameter=*/returnTypeIdent,
                                resultGenericParamDecl->getInterfaceType(), DC);
  ParameterList *doInvokeParamsList =
      ParameterList::create(C, {resultTyParamDecl});

  SmallVector<Requirement, 2> requirements;
  for (auto p : getDistributedSerializationRequirementProtocols(systemNominal, DAS)) {
    auto requirement =
        Requirement(RequirementKind::Conformance,
                    resultGenericParamDecl->getDeclaredInterfaceType(),
                    p->getDeclaredInterfaceType());
    requirements.push_back(requirement);
  }
  GenericSignature doInvokeGenSig =
      buildGenericSignature(C, parentFunc->getGenericSignature(),
                            {resultGenericParamDecl->getDeclaredInterfaceType()
                                 ->castTo<GenericTypeParamType>()},
                            std::move(requirements));

  FuncDecl *doInvokeOnReturnFunc = FuncDecl::createImplicit(
      C, swift::StaticSpellingKind::None,
      DeclName(C, doInvokeLocalFuncIdent, doInvokeParamsList),
      sloc,
      /*async=*/true,
      /*throws=*/true, doInvokeGenericParamList, doInvokeParamsList,
      /*returnType=*/C.TheEmptyTupleType, parentFunc);
  doInvokeOnReturnFunc->setImplicit();
  doInvokeOnReturnFunc->setSynthesized();
  doInvokeOnReturnFunc->setGenericSignature(doInvokeGenSig);

  auto *doInvokeContext = C.Allocate<DoInvokeOnReturnContext>();
  doInvokeContext->handlerParam = handlerParam;
  doInvokeContext->resultBufferParam = resultBufParam;
  doInvokeOnReturnFunc->setBodySynthesizer(
      deriveBodyDistributed_doInvokeOnReturn, doInvokeContext);

  return doInvokeOnReturnFunc;
}

static std::pair<BraceStmt *, bool>
deriveBodyDistributed_invokeHandlerOnReturn(AbstractFunctionDecl *afd,
                                            void *context) {
  auto implicit = true;
  ASTContext &C = afd->getASTContext();
  auto DC = afd->getDeclContext();
  auto DAS = C.getDistributedActorSystemDecl();

  // mock locations, we're a thunk and don't really need detailed locations
  const SourceLoc sloc = SourceLoc();
  const DeclNameLoc dloc = DeclNameLoc();

  NominalTypeDecl *nominal = dyn_cast<NominalTypeDecl>(DC);
  assert(nominal);

  auto func = dyn_cast<FuncDecl>(afd);
  assert(func);

  // === parameters
  auto params = func->getParameters();
  assert(params->size() == 3);
  auto handlerParam = params->get(0);
  auto resultBufParam = params->get(1);
  auto metatypeParam = params->get(2);

  auto serializationRequirementTypeTy =
      getDistributedSerializationRequirementType(nominal, DAS);

  auto serializationRequirementMetaTypeTy =
      ExistentialMetatypeType::get(serializationRequirementTypeTy);

  // Statements
  SmallVector<ASTNode, 8> stmts;

  // --- `let m = metatype as! SerializationRequirement.Type`
  VarDecl *metatypeVar =
      new (C) VarDecl(/*isStatic=*/false, VarDecl::Introducer::Let, sloc,
                      C.getIdentifier("m"), func);
  {
    metatypeVar->setImplicit();
    metatypeVar->setSynthesized();

    // metatype as! <<concrete SerializationRequirement.Type>>
    auto metatypeRef =
        new (C) DeclRefExpr(ConcreteDeclRef(metatypeParam), dloc, implicit);
    auto metatypeSRCastExpr = ForcedCheckedCastExpr::createImplicit(
        C, metatypeRef, serializationRequirementMetaTypeTy);

    auto metatypePattern = NamedPattern::createImplicit(C, metatypeVar);
    auto metatypePB = PatternBindingDecl::createImplicit(
        C, swift::StaticSpellingKind::None, metatypePattern,
        /*expr=*/metatypeSRCastExpr, func);

    stmts.push_back(metatypePB);
    stmts.push_back(metatypeVar);
  }

  // --- Declare the local function `doInvokeOnReturn`...
  FuncDecl *doInvokeOnReturnFunc = createLocalFunc_doInvokeOnReturn(
      C, func,
      nominal, handlerParam, resultBufParam);
  stmts.push_back(doInvokeOnReturnFunc);

  // --- try await _openExistential(metatypeVar, do: <<doInvokeLocalFunc>>)
  {
    auto openExistentialBaseIdent = C.getIdentifier("_openExistential");
    auto doIdent = C.getIdentifier("do");

    auto openExArgs = ArgumentList::createImplicit(
        C, {
               Argument(sloc, Identifier(),
                        new (C) DeclRefExpr(ConcreteDeclRef(metatypeVar), dloc,
                                            implicit)),
               Argument(sloc, doIdent,
                        new (C) DeclRefExpr(ConcreteDeclRef(doInvokeOnReturnFunc),
                                            dloc, implicit)),
           });
    Expr *tryAwaitDoOpenExistential =
        CallExpr::createImplicit(C,
                                 UnresolvedDeclRefExpr::createImplicit(
                                     C, openExistentialBaseIdent),
                                 openExArgs);

    tryAwaitDoOpenExistential =
        AwaitExpr::createImplicit(C, sloc, tryAwaitDoOpenExistential);
    tryAwaitDoOpenExistential =
        TryExpr::createImplicit(C, sloc, tryAwaitDoOpenExistential);

    stmts.push_back(tryAwaitDoOpenExistential);
  }

  auto body = BraceStmt::create(C, sloc, {stmts}, sloc, implicit);
  return {body, /*isTypeChecked=*/false};
}

/// Synthesizes the
///
/// \verbatim
/// static func invokeHandlerOnReturn(
////    handler: ResultHandler,
////    resultBuffer: UnsafeRawPointer,
////    metatype _metatype: Any.Type
////  ) async throws
/// \endverbatim
static FuncDecl *deriveDistributedActorSystem_invokeHandlerOnReturn(
    DerivedConformance &derived) {
  auto system = derived.Nominal;
  auto &C = system->getASTContext();

  // auto serializationRequirementType = getDistributedActorSystemType(decl);
  auto resultHandlerType = getDistributedActorSystemResultHandlerType(system);
  auto unsafeRawPointerType = C.getUnsafeRawPointerType();
  auto anyTypeType = ExistentialMetatypeType::get(C.TheAnyType); // Any.Type

  //  auto serializationRequirementType =
  //  getDistributedSerializationRequirementType(system, DAS);

  // params:
  // - handler: Self.ResultHandler
  // - resultBuffer:
  // - metatype _metatype: Any.Type
  auto *params = ParameterList::create(
      C,
      /*LParenLoc=*/SourceLoc(),
      /*params=*/
      {
          ParamDecl::createImplicit(
              C, C.Id_handler, C.Id_handler,
              system->mapTypeIntoContext(resultHandlerType), system),
          ParamDecl::createImplicit(
              C, C.Id_resultBuffer, C.Id_resultBuffer,
              unsafeRawPointerType, system),
          ParamDecl::createImplicit(
              C, C.Id_metatype, C.Id_metatype,
              anyTypeType, system)
      },
      /*RParenLoc=*/SourceLoc());

  // Func name: invokeHandlerOnReturn(handler:resultBuffer:metatype)
  DeclName name(C, C.Id_invokeHandlerOnReturn, params);

  // Expected type: (Self.ResultHandler, UnsafeRawPointer, any Any.Type) async
  // throws -> ()
  auto *funcDecl =
      FuncDecl::createImplicit(C, StaticSpellingKind::None, name, SourceLoc(),
                               /*async=*/true,
                               /*throws=*/true,
                               /*genericParams=*/nullptr, params,
                               /*returnType*/ TupleType::getEmpty(C), system);
  funcDecl->setSynthesized(true);
  funcDecl->copyFormalAccessFrom(system, /*sourceIsParentContext=*/true);
  funcDecl->setBodySynthesizer(deriveBodyDistributed_invokeHandlerOnReturn);

  derived.addMembersToConformanceContext({funcDecl});
  return funcDecl;
}

/******************************************************************************/
/******************************* PROPERTIES ***********************************/
/******************************************************************************/

// TODO(distributed): make use of this after all, but FORCE it?
static ValueDecl *deriveDistributedActor_id(DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // nonisolated let id: Self.ID // Self.ActorSystem.ActorID
  // ```
  auto propertyType = getDistributedActorIDType(derived.Nominal);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      DerivedConformance::SynthesizedIntroducer::Let, C.Id_id, propertyType,
      propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  derived.addMemberToConformanceContext(pbDecl, /*insertAtHead=*/true);
  derived.addMemberToConformanceContext(propDecl, /*insertAtHead=*/true);
  return propDecl;
}

static ValueDecl *deriveDistributedActor_actorSystem(
    DerivedConformance &derived) {
  auto &C = derived.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  assert(classDecl && derived.Nominal->isDistributedActor());

  // ```
  // nonisolated let actorSystem: ActorSystem
  // ```
  // (no need for @actorIndependent because it is an immutable let)
  auto propertyType = getDistributedActorSystemType(classDecl);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      DerivedConformance::SynthesizedIntroducer::Let, C.Id_actorSystem,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  // IMPORTANT: `id` MUST be the first field of a distributed actor, and
  // `actorSystem` MUST be the second field, because for a remote instance
  // we don't allocate memory after those two fields, so their order is very
  // important. The `hint` below makes sure the system is inserted right after.
  if (auto id = derived.Nominal->getDistributedActorIDProperty()) {
    derived.addMemberToConformanceContext(pbDecl, /*hint=*/id);
    derived.addMemberToConformanceContext(propDecl, /*hint=*/id);
  } else {
    // `id` will be synthesized next, and will insert at head,
    // so in order for system to be SECOND (as it must be),
    // we'll insert at head right now and as id gets synthesized we'll get
    // the correct order: id, actorSystem.
    derived.addMemberToConformanceContext(pbDecl, /*insertAtHead==*/true);
    derived.addMemberToConformanceContext(propDecl, /*insertAtHead=*/true);
  }

  return propDecl;
}

/******************************************************************************/
/***************************** ASSOC TYPES ************************************/
/******************************************************************************/

static Type
deriveDistributedActorType_ActorSystem(
    DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // Look for a type DefaultDistributedActorSystem within the parent context.
  auto defaultDistributedActorSystemLookup = TypeChecker::lookupUnqualified(
      derived.getConformanceContext()->getModuleScopeContext(),
      DeclNameRef(C.Id_DefaultDistributedActorSystem),
      derived.ConformanceDecl->getLoc());
  TypeDecl *defaultDistributedActorSystemTypeDecl = nullptr;
  for (const auto &found : defaultDistributedActorSystemLookup) {
    if (auto foundType = dyn_cast_or_null<TypeDecl>(found.getValueDecl())) {
      if (defaultDistributedActorSystemTypeDecl) {
        // Note: ambiguity, for now just fail.
        return nullptr;
      }

      defaultDistributedActorSystemTypeDecl = foundType;
      continue;
    }
  }

  // There is no default, so fail to synthesize.
  if (!defaultDistributedActorSystemTypeDecl)
    return nullptr;

  // Return the default system type.
  return defaultDistributedActorSystemTypeDecl->getDeclaredInterfaceType();
}

static Type
deriveDistributedActorType_ID(
    DerivedConformance &derived) {
  if (!derived.Nominal->isDistributedActor())
    return nullptr;

  // Look for a type DefaultDistributedActorSystem within the parent context.
  auto systemTy = getDistributedActorSystemType(derived.Nominal);

  // There is no known actor system type, so fail to synthesize.
  if (!systemTy || systemTy->hasError())
    return nullptr;

  if (auto systemNominal = systemTy->getAnyNominal()) {
    return getDistributedActorSystemActorIDType(systemNominal);
  }

  return nullptr;
}

static Type
deriveDistributedActorType_SerializationRequirement(
    DerivedConformance &derived) {
  if (!derived.Nominal->isDistributedActor())
    return nullptr;

  // Look for a type DefaultDistributedActorSystem within the parent context.
  auto systemTy = getDistributedActorSystemType(derived.Nominal);

  // There is no known actor system type, so fail to synthesize.
  if (!systemTy || systemTy->hasError())
    return nullptr;

  auto DAS = derived.Context.getDistributedActorSystemDecl();
  if (!DAS)
    return nullptr;

  if (auto systemNominal = systemTy->getAnyNominal())
    return getDistributedSerializationRequirementType(systemNominal, DAS);

  return nullptr;
}

/******************************************************************************/
/**************************** ENTRY POINTS ************************************/
/******************************************************************************/

// !!!!!!!!!!!!! IMPORTANT WHEN MAKING CHANGES TO REQUIREMENTS !!!!!!!!!!!!!!!!!
// !! Remember to update DerivedConformance::getDerivableRequirement          !!
// !! any time the signatures or list of derived requirements change.         !!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  if (auto var = dyn_cast<VarDecl>(requirement)) {
    if (var->getName() == Context.Id_id)
      return deriveDistributedActor_id(*this);

    if (var->getName() == Context.Id_actorSystem)
      return deriveDistributedActor_actorSystem(*this);
  }

  if (auto func = dyn_cast<FuncDecl>(requirement)) {
    // just a simple name check is enough here,
    // if we are invoked here we know for sure it is for the "right" function
    if (func->getName().getBaseName() == Context.Id_resolve) {
      return deriveDistributedActor_resolve(*this);
    }
  }

  return nullptr;
}

std::pair<Type, TypeDecl *> DerivedConformance::deriveDistributedActor(
    AssociatedTypeDecl *assocType) {
  if (!canDeriveDistributedActor(Nominal, cast<DeclContext>(ConformanceDecl)))
    return std::make_pair(Type(), nullptr);

  if (assocType->getName() == Context.Id_ActorSystem) {
    return std::make_pair(deriveDistributedActorType_ActorSystem(*this),
                          nullptr);
  }

  if (assocType->getName() == Context.Id_SerializationRequirement) {
    return std::make_pair(
        deriveDistributedActorType_SerializationRequirement(*this), nullptr);
  }

  if (assocType->getName() == Context.Id_ID) {
    return std::make_pair(deriveDistributedActorType_ID(*this), nullptr);
  }

  Context.Diags.diagnose(assocType->getLoc(),
                         diag::broken_distributed_actor_requirement);
  return std::make_pair(Type(), nullptr);
}

ValueDecl *
DerivedConformance::deriveDistributedActorSystem(ValueDecl *requirement) {
  if (auto func = dyn_cast<FuncDecl>(requirement)) {
    // just a simple name check is enough here,
    // if we are invoked here we know for sure it is for the "right" function
    if (func->getName().getBaseName() == Context.Id_invokeHandlerOnReturn) {
      return deriveDistributedActorSystem_invokeHandlerOnReturn(*this);
    }
  }

  return nullptr;
}

/******************************************************************************/
/*************************** ERRORS & DIAGNOSTICS *****************************/
/******************************************************************************/

void DerivedConformance::tryDiagnoseFailedDistributedActorDerivation(
        DeclContext *DC, NominalTypeDecl *nominal) {
  // TODO(distributed): offer better diagnosis for error scenarios here
}

void DerivedConformance::tryDiagnoseFailedDistributedActorSystemDerivation(
    DeclContext *DC, NominalTypeDecl *nominal) {
  // TODO(distributed): offer better diagnosis for error scenarios here
}
