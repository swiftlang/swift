//===-- DistributedActor.cpp - SIL utils for distributed actors -*- C++ -*-===//
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

#include "swift/SILOptimizer/Utils/DistributedActor.h"

#include "swift/AST/Decl.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILLocation.h"

namespace swift {

// MARK: utilities

SILValue loadActorTransport(SILBuilder &B, SILLocation loc,
                            ClassDecl *actorDecl, SILValue actorSelf) {
  assert(actorDecl->isDistributedActor());

  // get the VarDecl corresponding to the transport
  auto &C = actorDecl->getASTContext();
  auto refs = actorDecl->lookupDirect(C.Id_actorTransport);
  assert(refs.size() == 1);
  VarDecl *prop = dyn_cast<VarDecl>(refs.front());

  // form a reference and load it
  auto &F = B.getFunction();
  auto fieldAddr = B.createRefElementAddr(
      loc, actorSelf, prop, F.getLoweredType(prop->getInterfaceType()));
  return B.createLoad(loc, fieldAddr, LoadOwnershipQualifier::Copy);
}

// MARK: exposed interface

void emitActorReadyCall(SILBuilder &B, SILLocation loc, ClassDecl *actorDecl,
                        SILValue actor, SILValue transport) {

  auto &F = B.getFunction();
  auto &M = B.getModule();
  auto &C = actorDecl->getASTContext();

  ProtocolDecl *actorProto = C.getProtocol(KnownProtocolKind::DistributedActor);
  ProtocolDecl *transProto = C.getProtocol(KnownProtocolKind::ActorTransport);
  assert(actorProto);
  assert(transProto);

  // Open the transport existential
  auto transportASTType = transport->getType().getASTType();
  if (transportASTType->isAnyExistentialType()) {
    OpenedArchetypeType *Opened;
    transportASTType =
        transportASTType->openAnyExistentialType(Opened)->getCanonicalType();
    transport = B.createOpenExistentialAddr(loc, transport,
                                            F.getLoweredType(transportASTType),
                                            OpenedExistentialAccess::Immutable);
  }

  // Make the transport.actorReady call

  // the conformance here is just an abstract thing so we can simplify
  auto transportConfRef = ProtocolConformanceRef(transProto);
  assert(!transportConfRef.isInvalid() &&
         "Missing conformance to `ActorTransport`");

  auto *selfTyDecl = actorDecl->getSelfNominalTypeDecl();
  auto selfTy = F.mapTypeIntoContext(
      selfTyDecl->getDeclaredInterfaceType()); // TODO: thats just self var devl
                                               // getType

  // Note: it does not matter on what module we perform the lookup,
  // it is currently ignored. So the Stdlib module is good enough.
  auto *module = M.getSwiftModule();
  auto distributedActorConfRef = module->lookupConformance(selfTy, actorProto);
  assert(!distributedActorConfRef.isInvalid() &&
         "Missing conformance to `DistributedActor`");

  // Prepare the actorReady function
  auto actorReadyMethod =
      cast<FuncDecl>(transProto->getSingleRequirement(C.Id_actorReady));
  auto actorReadyRef = SILDeclRef(actorReadyMethod, SILDeclRef::Kind::Func);
  auto actorReadySILTy =
      M.Types.getConstantInfo(B.getTypeExpansionContext(), actorReadyRef)
          .getSILType();

  auto readyWitnessMethod =
      B.createWitnessMethod(loc,
                            /*lookupTy*/ transportASTType,
                            /*Conformance*/ transportConfRef,
                            /*member*/ actorReadyRef,
                            /*methodTy*/ actorReadySILTy);

  // prepare conformance substitutions
  auto genericSig = actorReadyMethod->getGenericSignature();

  SubstitutionMap subs =
      SubstitutionMap::get(genericSig, {transportASTType, selfTy},
                           {transportConfRef, distributedActorConfRef});

  B.createApply(loc, readyWitnessMethod, subs, {actor, transport});
}

} // namespace swift
