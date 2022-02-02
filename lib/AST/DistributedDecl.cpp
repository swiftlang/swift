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

#include "swift/AST/Decl.h"
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

bool AbstractFunctionDecl::isDistributedActorSystemRemoteCall(bool isVoidReturn) const {
  auto &C = getASTContext();
  auto module = getParentModule();

  // === Check the name
  auto callId = isVoidReturn ? C.Id_remoteCallVoid : C.Id_remoteCall;
  if (getBaseName() != callId)
    return false;

  // === Must be declared in a 'DistributedActorSystem' conforming type
  ProtocolDecl *distActorSystemProto =
      C.getProtocol(KnownProtocolKind::DistributedActorSystem);

  auto Nominal = getDeclContext()->getSelfNominalTypeDecl();
  auto distSystemConformance = module->lookupConformance(
      Nominal->getDeclaredInterfaceType(), distActorSystemProto);

  if (distSystemConformance.isInvalid())
    return false;

  // === Structural Checks
  // -- Must be throwing
  if (!hasThrows())
    return false;

  // -- Must be async
  if (!hasAsync())
    return false;

  // === Check generics
  if (!isGeneric())
    return false;

  // --- Check number of generic parameters
  auto genericParams = getGenericParams();
  unsigned int expectedGenericParamNum = isVoidReturn ? 2 : 3;

  if (genericParams->size() != expectedGenericParamNum)
    return false;

  // === Get the SerializationRequirement
//  auto SerializationRequirementTy =
//      C.getDistributedSerializationRequirementType(Nominal);


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
    return false;
  }

  // --- Check parameter: on: Actor
  auto actorParam = params->get(0);
  if (actorParam->getArgumentName() != C.Id_on)
    return false;

  // --- Check parameter: target RemoteCallTarget
  auto targetParam = params->get(1);
  if (targetParam->getArgumentName() != C.Id_target)
    return false;

  // --- Check parameter: invocation InvocationEncoder
  // FIXME: NOT INOUT, but we crash today if not
  auto invocationParam = params->get(2);
  if (invocationParam->getArgumentName() != C.Id_invocation)
    return false;

  // --- Check parameter: throwing: Err.Type
  auto thrownTypeParam = params->get(3);
  if (invocationParam->getArgumentName() != C.Id_invocation ||
      thrownTypeParam->getArgumentName() != C.Id_throwing) {
    return false;
  }

  // --- Check parameter: returning: Res.Type
  if (!isVoidReturn) {
    auto returnedTypeParam = params->get(4);
    if (returnedTypeParam->getArgumentName() != C.Id_returning) {
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
    return false;
  }

  return true;
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
