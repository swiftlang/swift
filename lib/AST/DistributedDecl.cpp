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
  return C.getAssociatedTypeOfDistributedSystem(actor, C.Id_ActorID);
}

Type swift::getDistributedActorSystemSerializationRequirementType(NominalTypeDecl *system) {
  assert(!system->isDistributedActor());
  auto &ctx = system->getASTContext();

  auto protocol = ctx.getProtocol(KnownProtocolKind::DistributedActorSystem);
  if (!protocol)
    return Type();

  // Dig out the serialization requirement type.
  auto module = system->getParentModule();
  Type selfType = system->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, protocol);
  return conformance.getTypeWitnessByName(selfType, ctx.Id_SerializationRequirement);
}

Type ASTContext::getAssociatedTypeOfDistributedSystem(NominalTypeDecl *actor,
                                                      Identifier member) {
  auto &ctx = actor->getASTContext();
  assert(actor->isDistributedActor() &&
         "Function intended to be used with distributed actor type");

  auto actorProtocol = ctx.getProtocol(KnownProtocolKind::DistributedActor);
  if (!actorProtocol)
    return ErrorType::get(ctx);

  AssociatedTypeDecl *actorSystemDecl =
      actorProtocol->getAssociatedType(ctx.Id_ActorSystem);
  if (!actorSystemDecl)
    return ErrorType::get(ctx);

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

Type ASTContext::getDistributedSerializationRequirementType(
    NominalTypeDecl *nominal) {
  return getAssociatedTypeOfDistributedSystem(nominal,
                                              Id_SerializationRequirement);
}

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
  fprintf(stderr, "[%s:%d] (%s) SYSTEM ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n", __FILE__, __LINE__, __FUNCTION__);
  systemNominal->dump();
  fprintf(stderr, "[%s:%d] (%s) SYSTEM ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n", __FILE__, __LINE__, __FUNCTION__);
//  auto SerializationRequirementTy =
//      systemProto->getAssociatedType(C.Id_SerializationRequirement);
  auto SerializationRequirementTy = // existential
      getDistributedActorSystemSerializationRequirementType(systemNominal);
//  auto SerializationRequirementTy = // existential
//      getDistributedSerializationRequirementProtocols(systemNominal);
  fprintf(stderr, "[%s:%d] (%s) REQ VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV\n", __FILE__, __LINE__, __FUNCTION__);
  SerializationRequirementTy->dump();
  fprintf(stderr, "[%s:%d] (%s) REQ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n", __FILE__, __LINE__, __FUNCTION__);
  if (SerializationRequirementTy->hasError()) {
    fprintf(stderr, "[%s:%d] (%s) if (SerializationRequirementTy->hasError())\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  fprintf(stderr, "[%s:%d] (%s) ---------------------\n", __FILE__, __LINE__, __FUNCTION__);

  llvm::SmallPtrSet<ProtocolDecl*, 2> serializationReqs;
      auto serialReqType = SerializationRequirementTy->castTo<ExistentialType>()
                               ->getConstraintType()
                               ->getDesugaredType();
      auto flattenedRequirements =
          flattenDistributedSerializationTypeToRequiredProtocols(
              serialReqType);
      for (auto p : flattenedRequirements) {
        fprintf(stderr, "[%s:%d] (%s) PROTO %s\n", __FILE__, __LINE__, __FUNCTION__, p->getNameStr().str().c_str());
        serializationReqs.insert(p);
      }

//  auto flattenedRequirements =
//      flattenDistributedSerializationTypeToRequiredProtocols(SerializationRequirementTy);
//  extractDistributedSerializationRequirements(
//      C, serializationReqNominal->getGenericRequirements());

  // -- Check number of generic requirements
  size_t expectedRequirementsNum = 3;
  if (!isVoidReturn) {
    // TODO(distributed): support alternative SerializationRequirements here
    auto serializationRequirementsNum = 2; // Codable == Encodable, Decodable
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
    fprintf(stderr, "[%s:%d] (%s) if (actorParam->getArgumentName() != C.Id_on)\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check parameter: target RemoteCallTarget
  auto targetParam = params->get(1);
  if (targetParam->getArgumentName() != C.Id_target) {
    fprintf(stderr, "[%s:%d] (%s) if (targetParam->getArgumentName() != C.Id_target)\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check parameter: invocation InvocationEncoder
  // FIXME: NOT INOUT, but we crash today if not
  auto invocationParam = params->get(2);
  if (invocationParam->getArgumentName() != C.Id_invocation) {
    fprintf(stderr, "[%s:%d] (%s) if (invocationParam->getArgumentName() != C.Id_invocation) {\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check parameter: throwing: Err.Type
  auto thrownTypeParam = params->get(3);
  if (thrownTypeParam->getArgumentName() != C.Id_throwing) {
    fprintf(stderr, "[%s:%d] (%s) if (thrownTypeParam->getArgumentName() != C.Id_throwing) {\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // --- Check parameter: returning: Res.Type
  if (!isVoidReturn) {
    auto returnedTypeParam = params->get(4);
    if (returnedTypeParam->getArgumentName() != C.Id_returning) {
      fprintf(stderr, "[%s:%d] (%s) if (thrownTypeParam->getArgumentName() != C.Id_throwing) {\n", __FILE__, __LINE__, __FUNCTION__);
      return false;
    }
  }

  // === Check generic parameters in detail
  // --- Check: Act: DistributedActor,
  //            Act.ID == Self.ActorID
  GenericTypeParamDecl *ActParam = genericParams->getParams()[0];

  // --- Check: Err: Error
  GenericTypeParamDecl *ErrParam = genericParams->getParams()[1];
  module->lookupConformance(
      mapTypeIntoContext(ErrParam->getDeclaredInterfaceType()),
      C.getProtocol(KnownProtocolKind::Error));

  /// --- Check: Res: SerializationRequirement
  GenericTypeParamDecl *ResParam =
      isVoidReturn ? nullptr : genericParams->getParams().back();


  auto sig = getGenericSignature();
  auto requirements = sig.getRequirements();
  if (requirements.size() != expectedRequirementsNum) {
    fprintf(stderr, "[%s:%d] (%s) if (requirements.size() != expectedRequirementsNum)\n", __FILE__, __LINE__, __FUNCTION__);
    return false;
  }

  // conforms_to: Act DistributedActor
  // conforms_to: Err Error
  // --- all the Res requirements ---
  // conforms_to: Res Decodable
  // conforms_to: Res Encodable
  // ...
  // --------------------------------
  // same_type: Act.ID FakeActorSystem.ActorID // LAST one
  auto actorReq = requirements[0];
  auto errorReq = requirements[1];
  auto actorIDReq = requirements.back();

  bool ActConformsDistributedActor = false;
  bool ErrConformsError = false;
  bool ResConformsSerializationRequirement = false;
  bool ActIDIsSameSystemID = false;

  auto distActorTy = C.getProtocol(KnownProtocolKind::DistributedActor)
                         ->getInterfaceType()
                         ->getMetatypeInstanceType();
  if (actorReq.getSecondType()->isEqual(distActorTy)) {
    ActConformsDistributedActor = actorReq.getKind() == RequirementKind::Conformance;
  }

  auto errorTy = C.getProtocol(KnownProtocolKind::Error)
                     ->getInterfaceType()
                     ->getMetatypeInstanceType();
  if (errorReq.getSecondType()->isEqual(errorTy)) {
    ErrConformsError = errorReq.getKind() == RequirementKind::Conformance;
  }

  if (isVoidReturn) {
    if (auto func = dyn_cast<FuncDecl>(this)) {
      if (func->getResultInterfaceType()->isVoid()) {
        ResConformsSerializationRequirement = true;
      }
    }
  } else {
    // FIXME(distributed): implement checking return type for serialization requirement
    assert(ResParam && "Non void function, yet no Res generic parameter found");
    if (auto func = dyn_cast<FuncDecl>(this)) {
      auto resultType = func->getResultInterfaceType();
      // The result of the function must be the `Res` generic argument.
      if (resultType->getMetatypeInstanceType()->isEqual(
              ResParam->getInterfaceType()->getMetatypeInstanceType())) {
        // FIXME: this is too simple, it has to check the SerializationRequirement is applied
        ResConformsSerializationRequirement = true;
      }
    }
  }

  // FIXME(distributed): implement checking ActIDIsSameSystemID

  // If any of the requirements is off, we fail the lookup
  if (!ActConformsDistributedActor ||
        !ErrConformsError ||
        !ResConformsSerializationRequirement
//      || !ActIDIsSameSystemID
        ) {
    fprintf(stderr, "[%s:%d] (%s) // If any of the requirements is off, we fail the lookup\n", __FILE__, __LINE__, __FUNCTION__);
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


bool AbstractFunctionDecl::isDistributed() const {
  return getAttrs().hasAttribute<DistributedActorAttr>();
}

ConstructorDecl*
NominalTypeDecl::getDistributedRemoteCallTargetInitFunction() const {
  auto &C = this->getASTContext();

  // FIXME(distributed): implement more properly... do with caching etc
  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  for (auto value : mutableThis->getMembers()) {
    auto ctor = dyn_cast<ConstructorDecl>(value);
    if (!ctor)
      continue;

    auto params = ctor->getParameters();
    if (params->size() != 1)
      return nullptr;

    if (params->get(0)->getArgumentName() == C.getIdentifier("_mangledName"))
      return ctor;

    return nullptr;
  }

  // TODO(distributed): make a Request for it?
  return nullptr;
}

VarDecl*
NominalTypeDecl::getDistributedActorSystemProperty() const {
  if (!this->isDistributedActor())
    return nullptr;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      GetDistributedActorSystemPropertyRequest{mutableThis},
      nullptr);
}

VarDecl*
NominalTypeDecl::getDistributedActorIDProperty() const {
  if (!this->isDistributedActor())
    return nullptr;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      GetDistributedActorIDPropertyRequest{mutableThis},
      nullptr);
}
