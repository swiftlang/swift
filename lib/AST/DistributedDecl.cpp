//===--- Decl.cpp - Swift Language Decl ASTs ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DistributedDecl.h"
#include "swift/AST/AccessRequests.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Lexer.h" // FIXME: Bad dependency
#include "clang/Lex/MacroInfo.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/StringExtras.h"

#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Module.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"

#include "InlinableText.h"
#include <algorithm>

using namespace swift;

/******************************************************************************/
/************** Distributed Actor System Associated Types *********************/
/******************************************************************************/

Type swift::getDistributedActorSystemType(NominalTypeDecl *actor) {
  assert(actor->isDistributedActor());
  auto &ctx = actor->getASTContext();

  auto protocol = ctx.getProtocol(KnownProtocolKind::DistributedActor);
  if (!protocol)
    return ErrorType::get(ctx);

  // Dig out the actor system type.
  auto module = actor->getParentModule();
  Type selfType = actor->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, protocol);
  return conformance.getTypeWitnessByName(selfType, ctx.Id_ActorSystem);
}

Type swift::getDistributedActorIDType(NominalTypeDecl *actor) {
  auto &C = actor->getASTContext();
  return C.getAssociatedTypeOfDistributedSystemOfActor(actor, C.Id_ActorID);
}

Type swift::getDistributedActorSystemActorIDRequirementType(NominalTypeDecl *system) {
  assert(!system->isDistributedActor());
  auto &ctx = system->getASTContext();

  auto protocol = ctx.getProtocol(KnownProtocolKind::DistributedActorSystem);
  if (!protocol)
    return Type();

  // Dig out the serialization requirement type.
  auto module = system->getParentModule();
  Type selfType = system->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, protocol);
  return conformance.getTypeWitnessByName(selfType, ctx.Id_ActorID);
}

Type swift::getDistributedSerializationRequirementType(
    NominalTypeDecl *nominal, ProtocolDecl *protocol) {
  assert(!nominal->isDistributedActor());
  assert(protocol);
  auto &ctx = nominal->getASTContext();

  // Dig out the serialization requirement type.
  auto module = nominal->getParentModule();
  Type selfType = nominal->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, protocol);
  if (conformance.isInvalid())
    return Type();

  return conformance.getTypeWitnessByName(selfType, ctx.Id_SerializationRequirement);
}

Type ASTContext::getAssociatedTypeOfDistributedSystemOfActor(
    NominalTypeDecl *actor, Identifier member) {
  auto &ctx = actor->getASTContext();

  auto actorProtocol = ctx.getProtocol(KnownProtocolKind::DistributedActor);
  if (!actorProtocol)
    return ErrorType::get(ctx);

  AssociatedTypeDecl *actorSystemDecl =
      actorProtocol->getAssociatedType(ctx.Id_ActorSystem);
  if (!actorSystemDecl)
    return Type();

  auto actorSystemProtocol = ctx.getProtocol(KnownProtocolKind::DistributedActorSystem);
  if (!actorSystemProtocol)
    return Type();

  AssociatedTypeDecl *assocTypeDecl =
      actorSystemProtocol->getAssociatedType(member);
  if (!assocTypeDecl)
    return Type();

  auto module = actor->getParentModule();
  Type selfType = actor->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, actorProtocol);
  Type dependentType = actorProtocol->getSelfInterfaceType();
  dependentType = DependentMemberType::get(dependentType, actorSystemDecl);
  dependentType = DependentMemberType::get(dependentType, assocTypeDecl);

  return dependentType.subst(SubstitutionMap::getProtocolSubstitutions(
      actorProtocol, selfType, conformance));
}

//Type ASTContext::getDistributedSerializationRequirementType(
//    NominalTypeDecl *nominal) {
//  return getAssociatedTypeOfDistributedSystemOfActor(nominal,
//                                              Id_SerializationRequirement);
//}

FuncDecl*
ASTContext::getDistributedActorArgumentDecodingMethod(NominalTypeDecl *actor) {
  if (!actor->isDistributedActor())
    return nullptr;

  return evaluateOrDefault(
      evaluator, GetDistributedActorArgumentDecodingMethodRequest{actor}, nullptr);
}

NominalTypeDecl*
ASTContext::getDistributedActorInvocationDecoder(NominalTypeDecl *actor) {
  if (!actor->isDistributedActor())
    return nullptr;

  return evaluateOrDefault(
      evaluator, GetDistributedActorInvocationDecoderRequest{actor}, nullptr);
}

bool
swift::getDistributedActorSystemSerializationRequirements(
    NominalTypeDecl *nominal,
    ProtocolDecl *protocol,
    llvm::SmallPtrSetImpl<ProtocolDecl *> &requirementProtos) {
  auto existentialRequirementTy =
      getDistributedSerializationRequirementType(nominal, protocol);
  if (existentialRequirementTy->hasError()) {
    fprintf(stderr, "[%s:%d] (%s) if (SerializationRequirementTy->hasError())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  if (existentialRequirementTy->isAny())
    return true; // we're done here, any means there are no requirements

  fprintf(stderr, "[%s:%d] (%s) existentialRequirementTy\n", __FILE__, __LINE__, __FUNCTION__);
  existentialRequirementTy.dump();

  fprintf(stderr, "[%s:%d] (%s) ---------------------\n", __FILE__, __LINE__, __FUNCTION__);

  auto serialReqType = existentialRequirementTy->castTo<ExistentialType>()
                           ->getConstraintType()
                           ->getDesugaredType();
  fprintf(stderr, "[%s:%d] (%s) serialReqType serialReqType serialReqType serialReqType serialReqType serialReqType\n", __FILE__, __LINE__, __FUNCTION__);
  serialReqType->dump();
  auto flattenedRequirements =
      flattenDistributedSerializationTypeToRequiredProtocols(
          serialReqType);
  for (auto p : flattenedRequirements) {
    fprintf(stderr, "[%s:%d] (%s) PROTO %s\n", __FILE__, __LINE__, __FUNCTION__, p->getNameStr().str().c_str());
    requirementProtos.insert(p);
  }

  return true;
}

llvm::SmallPtrSet<ProtocolDecl *, 2>
swift::flattenDistributedSerializationTypeToRequiredProtocols(
    TypeBase *serializationRequirement) {
  llvm::SmallPtrSet<ProtocolDecl *, 2> serializationReqs;
  if (auto composition =
          serializationRequirement->getAs<ProtocolCompositionType>()) {
    for (auto member : composition->getMembers()) {
      if (auto comp = member->getAs<ProtocolCompositionType>()) {
        for (auto protocol :
             flattenDistributedSerializationTypeToRequiredProtocols(comp)) {
          serializationReqs.insert(protocol);
        }
      } else if (auto *protocol = member->getAs<ProtocolType>()) {
        serializationReqs.insert(protocol->getDecl());
      }
    }
  } else {
    auto protocol = serializationRequirement->castTo<ProtocolType>()->getDecl();
    serializationReqs.insert(protocol);
  }

  return serializationReqs;
}

bool swift::checkDistributedSerializationRequirementIsExactlyCodable(
    ASTContext &C,
    const llvm::SmallPtrSetImpl<ProtocolDecl *> &allRequirements) {
  auto encodable = C.getProtocol(KnownProtocolKind::Encodable);
  auto decodable = C.getProtocol(KnownProtocolKind::Decodable);

  if (allRequirements.size() != 2)
    return false;

  return allRequirements.count(encodable) &&
         allRequirements.count(decodable);
}

/******************************************************************************/
/********************* Ad-hoc protocol requirement checks *********************/
/******************************************************************************/

bool AbstractFunctionDecl::isDistributedActorSystemRemoteCall(bool isVoidReturn) const {
  auto &C = getASTContext();
  auto module = getParentModule();

  fprintf(stderr, "[%s:%d] (%s) ====================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ====================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ====================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ====================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) CHECK NAME: %s\n", __FILE__, __LINE__, __FUNCTION__, getNameStr().str().c_str());

  // === Check the name
  auto callId = isVoidReturn ? C.Id_remoteCallVoid : C.Id_remoteCall;
  if (getBaseName() != callId) {
    fprintf(stderr, "[%s:%d] (%s)   if (getBaseName() != callId)\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // === Must be declared in a 'DistributedActorSystem' conforming type
  ProtocolDecl *systemProto =
      C.getProtocol(KnownProtocolKind::DistributedActorSystem);

  auto systemNominal = getDeclContext()->getSelfNominalTypeDecl();
  auto distSystemConformance = module->lookupConformance(
      systemNominal->getDeclaredInterfaceType(), systemProto);

  if (distSystemConformance.isInvalid()) {
    fprintf(stderr, "[%s:%d] (%s) if (distSystemConformance.isInvalid())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // === Structural Checks
  // -- Must be throwing
  if (!hasThrows()) {
    fprintf(stderr, "[%s:%d] (%s) if (!hasThrows())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // -- Must be async
  if (!hasAsync()) {
    fprintf(stderr, "[%s:%d] (%s) if (!hasAsync())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // === Check generics
  if (!isGeneric()) {
    fprintf(stderr, "[%s:%d] (%s) if (!isGeneric())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check number of generic parameters
  auto genericParams = getGenericParams();
  unsigned int expectedGenericParamNum = isVoidReturn ? 2 : 3;

  if (genericParams->size() != expectedGenericParamNum) {
    fprintf(stderr, "[%s:%d] (%s) if (genericParams->size() != expectedGenericParamNum)\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // === Get the SerializationRequirement
  SmallPtrSet<ProtocolDecl*, 2> requirementProtos;
  if (!getDistributedActorSystemSerializationRequirements(
          systemNominal, systemProto, requirementProtos)) {
    return false;
  }

  // -- Check number of generic requirements
  size_t expectedRequirementsNum = 3;
  size_t serializationRequirementsNum = 0;
  if (!isVoidReturn) {
    serializationRequirementsNum = requirementProtos.size();
    expectedRequirementsNum += serializationRequirementsNum;
  }

  // === Check all parameters
  auto params = getParameters();

  // --- Count of parameters depends on if we're void returning or not
  unsigned int expectedParamNum = isVoidReturn ? 4 : 5;
  if (!params || params->size() != expectedParamNum) {
    fprintf(stderr, "[%s:%d] (%s) if (!params || params->size() != expectedParamNum) {\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check parameter: on: Actor
  auto actorParam = params->get(0);
  if (actorParam->getArgumentName() != C.Id_on) {
    return false;
  }

  // --- Check parameter: target RemoteCallTarget
  auto targetParam = params->get(1);
  if (targetParam->getArgumentName() != C.Id_target) {
    return false;
  }

  // --- Check parameter: invocation InvocationEncoder
  // FIXME: NOT INOUT, but we crash today if not
  auto invocationParam = params->get(2);
  if (invocationParam->getArgumentName() != C.Id_invocation) {
    return false;
  }

  // --- Check parameter: throwing: Err.Type
  auto thrownTypeParam = params->get(3);
  if (thrownTypeParam->getArgumentName() != C.Id_throwing) {
    return false;
  }

  // --- Check parameter: returning: Res.Type
  if (!isVoidReturn) {
    auto returnedTypeParam = params->get(4);
    if (returnedTypeParam->getArgumentName() != C.Id_returning) {
      return false;
    }
  }

  fprintf(stderr, "[%s:%d] (%s) GENERIC PARAMS\n", __FILE__, __LINE__, __FUNCTION__);
  for (auto param : genericParams->getParams()) {
    param->dump();
  }
  // === Check generic parameters in detail
  // --- Check: Act: DistributedActor,
  //            Act.ID == Self.ActorID
  GenericTypeParamDecl *ActParam = genericParams->getParams()[0];
  auto ActConformance = module->lookupConformance(
      mapTypeIntoContext(ActParam->getDeclaredInterfaceType()),
      C.getProtocol(KnownProtocolKind::DistributedActor));
  if (ActConformance.isInvalid()) {
    return false;
  }

  // --- Check: Err: Error
  GenericTypeParamDecl *ErrParam = genericParams->getParams()[1];
  auto ErrConformance = module->lookupConformance(
      mapTypeIntoContext(ErrParam->getDeclaredInterfaceType()),
      C.getProtocol(KnownProtocolKind::Error));
  if (ErrConformance.isInvalid()) {
    return false;
  }

  // --- Check: Res: SerializationRequirement
  // We could have the `SerializationRequirement = Any` in which case there are
  // no requirements to check on `Res`
  GenericTypeParamDecl *ResParam = nullptr;
  if (!isVoidReturn) {
    ResParam = genericParams->getParams().back();
  }

  auto sig = getGenericSignature();
  auto requirements = sig.getRequirements();

  fprintf(stderr, "[%s:%d] (%s) REQUIREMENTS requirements::::::::::::::::\n", __FILE__, __LINE__, __FUNCTION__);
  for (auto r : requirements) {
    r.dump();
  }
  fprintf(stderr, "[%s:%d] (%s) REQUIREMENT PROTOS ::::::::::::::::\n", __FILE__, __LINE__, __FUNCTION__);
  for (auto r : requirementProtos) {
    r->dump();
  }

  if (requirements.size() != expectedRequirementsNum) {
    return false;
  }

  // --- Check the expected requirements
  // conforms_to: Act DistributedActor
  // conforms_to: Err Error
  // --- all the Res requirements ---
  // conforms_to: Res Decodable
  // conforms_to: Res Encodable
  // ...
  // --------------------------------
  // same_type: Act.ID FakeActorSystem.ActorID // LAST one

  // --- Check requirement: conforms_to: Act DistributedActor
  auto actorReq = requirements[0];
  auto distActorTy = C.getProtocol(KnownProtocolKind::DistributedActor)
                       ->getInterfaceType()
                       ->getMetatypeInstanceType();
  if (actorReq.getKind() != RequirementKind::Conformance) {
    return false;
  }
  if (!actorReq.getSecondType()->isEqual(distActorTy)) {
    return false;
  }

  // --- Check requirement: conforms_to: Err Error
  auto errorReq = requirements[1];
  auto errorTy = C.getProtocol(KnownProtocolKind::Error)
                     ->getInterfaceType()
                     ->getMetatypeInstanceType();
  if (errorReq.getKind() != RequirementKind::Conformance) {
    return false;
  }
  if (!errorReq.getSecondType()->isEqual(errorTy)) {
    return false;
  }

  // --- Check requirement: Res either Void or all SerializationRequirements
  if (isVoidReturn) {
    if (auto func = dyn_cast<FuncDecl>(this)) {
      if (!func->getResultInterfaceType()->isVoid()) {
        return false;
      }
    }
  } else if (ResParam) {
    assert(ResParam && "Non void function, yet no Res generic parameter found");
    if (auto func = dyn_cast<FuncDecl>(this)) {
      auto resultType = func->mapTypeIntoContext(func->getResultInterfaceType())
                            ->getMetatypeInstanceType()
                            ->getDesugaredType();
      auto resultParamType = func->mapTypeIntoContext(
          ResParam->getInterfaceType()->getMetatypeInstanceType());
      // The result of the function must be the `Res` generic argument.
      if (!resultType->isEqual(resultParamType)) {
        return false;
      }

      for (auto requirementProto : requirementProtos) {
        auto conformance = module->lookupConformance(resultType, requirementProto);
        if (conformance.isInvalid()) {
          return false;
        }
      }
    } else {
      return false;
    }
  }

  // -- Check requirement: same_type Actor.ID Self.ActorID
  auto actorIdReq = requirements.back();
  if (actorIdReq.getKind() != RequirementKind::SameType) {
    return false;
  }
  auto expectedActorIdTy =
      getDistributedActorSystemActorIDRequirementType(systemNominal);
  actorIdReq.dump();
  if (!actorIdReq.getSecondType()->isEqual(expectedActorIdTy)) {
    return false;
  }

  return true;
}

bool
AbstractFunctionDecl::isDistributedTargetInvocationEncoderRecordArgument() const {
  auto &C = getASTContext();
  auto module = getParentModule();

  // === Check base name
  if (getBaseIdentifier() != C.Id_recordArgument)
    return false;

  // === Must be declared in a 'DistributedTargetInvocationEncoder' conforming type
  ProtocolDecl *encoderProto =
      C.getProtocol(KnownProtocolKind::DistributedTargetInvocationEncoder);

  auto encoderNominal = getDeclContext()->getSelfNominalTypeDecl();
  auto protocolConformance = module->lookupConformance(
      encoderNominal->getDeclaredInterfaceType(), encoderProto);

  if (protocolConformance.isInvalid()) {
    return false;
  }

  // === Check modifiers
  // --- must not be async
    if (hasAsync()) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // --- must be throwing
    if (!hasThrows()) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // === Check generics
    if (!isGeneric()) {
      fprintf(stderr, "[%s:%d] (%s) if (!isGeneric())\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // --- Check number of generic parameters
    auto genericParams = getGenericParams();
    unsigned int expectedGenericParamNum = 1;

    if (genericParams->size() != expectedGenericParamNum) {
      fprintf(stderr, "[%s:%d] (%s) if (genericParams->size() != expectedGenericParamNum)\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // === Get the SerializationRequirement
    SmallPtrSet<ProtocolDecl*, 2> requirementProtos;
    if (!getDistributedActorSystemSerializationRequirements(
            encoderNominal, encoderProto, requirementProtos)) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // -- Check number of generic requirements
    size_t serializationRequirementsNum = requirementProtos.size();
    size_t expectedRequirementsNum = serializationRequirementsNum;

    // === Check all parameters
    auto params = getParameters();
    if (params->size() != 1) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // --- Check parameter: _ argument
  auto argumentParam = params->get(0);
  if (!argumentParam->getArgumentName().is("_")) {
    fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // === Check generic parameters in detail
  // --- Check: Argument: SerializationRequirement
  GenericTypeParamDecl *ArgumentParam = genericParams->getParams()[0];

  auto sig = getGenericSignature();
  auto requirements = sig.getRequirements();

  if (requirements.size() != expectedRequirementsNum) {
    fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check the expected requirements
  // --- all the Argument requirements ---
  // conforms_to: Argument Decodable
  // conforms_to: Argument Encodable
  // ...

  auto func = dyn_cast<FuncDecl>(this);
  if (!func) {
    fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  auto resultType = func->mapTypeIntoContext(func->getResultInterfaceType())
                        ->getMetatypeInstanceType()
                        ->getDesugaredType();
  auto resultParamType = func->mapTypeIntoContext(
      ArgumentParam->getInterfaceType()->getMetatypeInstanceType());
  // The result of the function must be the `Res` generic argument.
  if (!resultType->isEqual(resultParamType)) {
    return false;
  }

  for (auto requirementProto : requirementProtos) {
    auto conformance = module->lookupConformance(resultType, requirementProto);
    if (conformance.isInvalid()) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }
  }

  // === Check result type: Void
  if (!func->getResultInterfaceType()->isVoid()) {
    fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  return true;
}

bool
AbstractFunctionDecl::isDistributedTargetInvocationEncoderRecordReturnType() const {
  auto &C = getASTContext();
  auto module = getParentModule();

  // === Check base name
  if (getBaseIdentifier() != C.Id_recordReturnType)
    return false;

  // === Must be declared in a 'DistributedTargetInvocationEncoder' conforming type
  ProtocolDecl *encoderProto =
      C.getProtocol(KnownProtocolKind::DistributedTargetInvocationEncoder);

  auto encoderNominal = getDeclContext()->getSelfNominalTypeDecl();
  auto protocolConformance = module->lookupConformance(
      encoderNominal->getDeclaredInterfaceType(), encoderProto);

  if (protocolConformance.isInvalid()) {
    return false;
  }

  // === Check modifiers
  // --- must not be async
  if (hasAsync()) {
    return false;
  }

  // --- must be throwing
  if (!hasThrows()) {
    return false;
  }

  // === Check generics
  if (!isGeneric()) {
    fprintf(stderr, "[%s:%d] (%s) if (!isGeneric())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check number of generic parameters
  auto genericParams = getGenericParams();
  unsigned int expectedGenericParamNum = 1;

  if (genericParams->size() != expectedGenericParamNum) {
    fprintf(stderr, "[%s:%d] (%s) if (genericParams->size() != expectedGenericParamNum)\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // === Get the SerializationRequirement
  SmallPtrSet<ProtocolDecl*, 2> requirementProtos;
  if (!getDistributedActorSystemSerializationRequirements(
          encoderNominal, encoderProto, requirementProtos)) {
    return false;
  }

  // -- Check number of generic requirements
  size_t serializationRequirementsNum = requirementProtos.size();
  size_t expectedRequirementsNum = serializationRequirementsNum;

  // === Check all parameters
  auto params = getParameters();
  if (params->size() != 1) {
    return false;
  }

  // --- Check parameter: _ argument
  auto argumentParam = params->get(0);
  if (!argumentParam->getArgumentName().is("_")) {
    return false;
  }

  // === Check generic parameters in detail
  // --- Check: Argument: SerializationRequirement
  GenericTypeParamDecl *ArgumentParam = genericParams->getParams()[0];

  auto sig = getGenericSignature();
  auto requirements = sig.getRequirements();

  if (requirements.size() != expectedRequirementsNum) {
    return false;
  }

  // --- Check the expected requirements
  // --- all the Argument requirements ---
  // conforms_to: Argument Decodable
  // conforms_to: Argument Encodable
  // ...

  auto func = dyn_cast<FuncDecl>(this);
  if (!func)
    return false;

  auto resultType = func->mapTypeIntoContext(func->getResultInterfaceType())
                        ->getMetatypeInstanceType()
                        ->getDesugaredType();
  auto resultParamType = func->mapTypeIntoContext(
      ArgumentParam->getInterfaceType()->getMetatypeInstanceType());
  // The result of the function must be the `Res` generic argument.
  if (!resultType->isEqual(resultParamType)) {
    return false;
  }

  for (auto requirementProto : requirementProtos) {
    auto conformance = module->lookupConformance(resultType, requirementProto);
    if (conformance.isInvalid()) {
      return false;
    }
  }

  // === Check result type: Void
  if (!func->getResultInterfaceType()->isVoid()) {
    return false;
  }

  return true;
}

bool
AbstractFunctionDecl::isDistributedTargetInvocationEncoderRecordErrorType() const {

    auto &C = getASTContext();
    auto module = getParentModule();

    // === Check base name
    if (getBaseIdentifier() != C.Id_recordReturnType)
      return false;

    // === Must be declared in a 'DistributedTargetInvocationEncoder' conforming type
    ProtocolDecl *encoderProto =
        C.getProtocol(KnownProtocolKind::DistributedTargetInvocationEncoder);

    auto encoderNominal = getDeclContext()->getSelfNominalTypeDecl();
    auto protocolConformance = module->lookupConformance(
        encoderNominal->getDeclaredInterfaceType(), encoderProto);

    if (protocolConformance.isInvalid()) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // === Check modifiers
    // --- must not be async
    if (hasAsync()) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // --- must be throwing
    if (!hasThrows()) {
      fprintf(stderr, "[%s:%d] (%s) return false\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // === Check generics
    if (!isGeneric()) {
      fprintf(stderr, "[%s:%d] (%s) if (!isGeneric())\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // --- Check number of generic parameters
    auto genericParams = getGenericParams();
    unsigned int expectedGenericParamNum = 1;

    if (genericParams->size() != expectedGenericParamNum) {
      fprintf(stderr, "[%s:%d] (%s) if (genericParams->size() != expectedGenericParamNum)\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }

    // === Check all parameters
    auto params = getParameters();
    if (params->size() != 1) {
      return false;
    }

    // --- Check parameter: _ argument
    auto argumentParam = params->get(0);
    if (!argumentParam->getArgumentName().is("_")) {
      return false;
    }

    // --- Check: Argument: SerializationRequirement
    auto sig = getGenericSignature();
    auto requirements = sig.getRequirements();
    if (requirements.size() != 1) {
      return false;
    }

    // === Check generic parameters in detail
    GenericTypeParamDecl *ArgumentParam = genericParams->getParams()[0];

    // --- Check requirement: conforms_to: Err Error
    auto errorReq = requirements[0];
    auto errorTy = C.getProtocol(KnownProtocolKind::Error)
                       ->getInterfaceType()
                       ->getMetatypeInstanceType();
    if (errorReq.getKind() != RequirementKind::Conformance) {
      return false;
    }
    if (!errorReq.getSecondType()->isEqual(errorTy)) {
      return false;
    }

    // === Check result type: Void
    auto func = dyn_cast<FuncDecl>(this);
    if (!func)
      return false;

    if (!func->getResultInterfaceType()->isVoid()) {
      return false;
    }

    return true;
}

llvm::SmallPtrSet<ProtocolDecl *, 2>
swift::extractDistributedSerializationRequirements(
    ASTContext &C, ArrayRef<Requirement> allRequirements) {
  llvm::SmallPtrSet<ProtocolDecl *, 2> serializationReqs;
  auto systemProto = C.getProtocol(KnownProtocolKind::DistributedActorSystem);
  auto serializationReqAssocType =
      systemProto->getAssociatedType(C.Id_SerializationRequirement);
  auto systemSerializationReqTy = serializationReqAssocType->getInterfaceType();

  for (auto req : allRequirements) {
    if (req.getSecondType()->isAny()) {
      continue;
    }
    if (!req.getFirstType()->hasDependentMember())
      continue;

    if (auto dependentMemberType =
            req.getFirstType()->castTo<DependentMemberType>()) {
      auto dependentTy =
          dependentMemberType->getAssocType()->getInterfaceType();

      if (dependentTy->isEqual(systemSerializationReqTy)) {
        auto requirementProto = req.getSecondType();
        if (auto proto = dyn_cast_or_null<ProtocolDecl>(
                requirementProto->getAnyNominal())) {
          serializationReqs.insert(proto);
        } else {
          auto serialReqType = requirementProto->castTo<ExistentialType>()
                                   ->getConstraintType()
                                   ->getDesugaredType();
          auto flattenedRequirements =
              flattenDistributedSerializationTypeToRequiredProtocols(
                  serialReqType);
          for (auto p : flattenedRequirements) {
            serializationReqs.insert(p);
          }
        }
      }
    }
  }

  return serializationReqs;
}

/******************************************************************************/
/********************** Distributed Functions *********************************/
/******************************************************************************/

bool AbstractFunctionDecl::isDistributed() const {
  return getAttrs().hasAttribute<DistributedActorAttr>();
}

ConstructorDecl *
NominalTypeDecl::getDistributedRemoteCallTargetInitFunction() const {
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      GetDistributedRemoteCallTargetInitFunctionRequest(mutableThis), nullptr);
}

/******************************************************************************/
/********************** Distributed Actor Properties **************************/
/******************************************************************************/

VarDecl*
NominalTypeDecl::getDistributedActorSystemProperty() const {
  if (!isDistributedActor())
    return nullptr;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      GetDistributedActorSystemPropertyRequest{mutableThis},
      nullptr);
}

VarDecl*
NominalTypeDecl::getDistributedActorIDProperty() const {
  if (!isDistributedActor())
    return nullptr;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      GetDistributedActorIDPropertyRequest{mutableThis},
      nullptr);
}
