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
#include "TypeCheckConcurrency.h"
#include "TypeCheckDistributed.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isDistributedActor() && dc == nominal;
}

/******************************************************************************/
/******************************* FUNCTIONS ************************************/
/******************************************************************************/


//// === Codable / Decode --------------------------------------------------------
//
///// Synthesizes the body for
/////
///// ```
///// nonisolated public func encode(to encoder: Encoder) throws {
/////   var container = encoder.singleValueContainer()
/////   try container.encode(self.id)
///// }
///// ```
/////
///// \param initDecl The function decl whose body to synthesize.
//static std::pair<BraceStmt *, bool>
//createDistributedActor_encode_toDecoder_body(AbstractFunctionDecl *initDecl, void *) {
//  auto *funcDC = cast<DeclContext>(initDecl);
//  auto &C = funcDC->getASTContext();
//
//  SmallVector<ASTNode, 4> statements;
//
//  auto decoderParam = initDecl->getParameters()->get(0);
//  auto *decoderExpr = new (C) DeclRefExpr(ConcreteDeclRef(decoderParam),
//                                          DeclNameLoc(), /*Implicit=*/true);
//
//  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);
//
//  auto *assignActorSelf = new (C) AssignExpr(
//      selfRef, SourceLoc(), decoderExpr, /*Implicit=*/true);
//  statements.push_back(assignActorSelf);
//  // end-of-FIXME: this must be checking with the transport instead
//
//  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
//                                 /*implicit=*/true);
//
//  return { body, /*isTypeChecked=*/false };
//}
//
//
//static FuncDecl *deriveDistributedActor_encode(DerivedConformance &derived) {
//  auto decl = dyn_cast<ClassDecl>(derived.Nominal);
//  assert(decl->isDistributedActor());
//
//  auto &C = decl->getASTContext();
//  auto addressType = getDistributedActorIdentityType(decl);
//  auto encoderType = C.getEncoderType();
//
//  auto mkParam = [&](Identifier argName, Identifier paramName, Type ty) -> ParamDecl* {
//    auto *param = new (C) ParamDecl(SourceLoc(),
//                                    SourceLoc(), argName,
//                                    SourceLoc(), paramName, decl);
//    param->setImplicit();
//    param->setSpecifier(ParamSpecifier::Default);
//    param->setInterfaceType(ty);
//    return param;
//  };
//
//  // (to encoder: Encoder)
//  auto *params = ParameterList::create(
//      C,
//      /*LParenLoc=*/SourceLoc(),
//      /*params=*/{ mkParam(C.Id_to, C.Id_encoder, encoderType) },
//      /*RParenLoc=*/SourceLoc()
//  );
//
//  // Func name: encode(to:)
//  DeclName name(C, C.Id_encode, params);
//
//  // Expected type: (Self) -> (Encoder) throws -> ()
//  auto *encodeDecl =
//      FuncDecl::createImplicit(C, StaticSpellingKind::None,
//                               name, SourceLoc(),
//                               /*async=*/false,
//                               /*throws=*/true,
//                               /*genericParams=*/nullptr,
//                               params,
//                               /*returnType*/decl->getDeclaredInterfaceType(),
//                               decl);
//  encodeDecl->setImplicit();
//  encodeDecl->setSynthesized();
//  encodeDecl->setBodySynthesizer(&createDistributedActor_encode_toDecoder_body);
//
//
//  encodeDecl->copyFormalAccessFrom(decl, /*sourceIsParentContext=*/true);
//
//  derived.addMembersToConformanceContext({encodeDecl});
//  return encodeDecl;
//}
//
//// === Codable / init(from:) ---------------------------------------------------
//
///// Synthesizes the body for
/////
///// ```
/////  nonisolated public init(from decoder: Decoder) throws {
/////   guard let transport = decoder.userInfo[.actorTransportKey] as? Transport else {
/////     throw DistributedActorCodingError(message:
/////       "Missing Transport (for key .actorTransportKey) " +
/////       "in Decoder.userInfo, while decoding \(Self.self).")
/////   }
/////
/////   let id: Identity = try transport.decodeIdentity(from: decoder)
/////   self = try Self.resolve(id, using: transport)
///// }
///// ```
/////
///// \param initDecl The function decl whose body to synthesize.
//static std::pair<BraceStmt *, bool>
//createDistributedActor_init_fromDecoder_body(AbstractFunctionDecl *initDecl, void *) {
//  auto *funcDC = cast<DeclContext>(initDecl);
//  auto &C = funcDC->getASTContext();
//
//  SmallVector<ASTNode, 4> statements;
//
//  auto decoderParam = initDecl->getParameters()->get(0);
//  auto *decoderExpr = new (C) DeclRefExpr(ConcreteDeclRef(decoderParam),
//                                          DeclNameLoc(), /*Implicit=*/true);
//
//  auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);
//
//  auto *assignActorSelf = new (C) AssignExpr(
//      selfRef, SourceLoc(), decoderExpr, /*Implicit=*/true);
//  statements.push_back(assignActorSelf);
//  // end-of-FIXME: this must be checking with the transport instead
//
//  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
//                                 /*implicit=*/true);
//
//  return { body, /*isTypeChecked=*/false };
//}
//
///// Synthesizes the
/////
///// ```
///// init(resolve address: ActorAddress, using transport: ActorTransport) throws
///// ```
/////
///// Decodable initializer.
//static ConstructorDecl *deriveDistributedActor_init_fromDecoder(DerivedConformance &derived) {
//  auto decl = dyn_cast<ClassDecl>(derived.Nominal);
//  assert(decl->isDistributedActor());
//  auto &C = decl->getASTContext();
//
//  // Expected type: (Self) -> (Decoder) -> (Self)
//  //
//  // Param: (from decoder: Decoder)
//  auto decoderTy = C.getDecoderDecl()->getDeclaredInterfaceType();
//  auto *addressParamDecl = new (C) ParamDecl(
//      SourceLoc(), SourceLoc(), C.Id_from,
//      SourceLoc(), C.Id_decoder, decl);
//  addressParamDecl->setImplicit();
//  addressParamDecl->setSpecifier(ParamSpecifier::Default);
//  addressParamDecl->setInterfaceType(decoderTy);
//
//  auto *paramList = ParameterList::create(
//      C,
//      /*LParenLoc=*/SourceLoc(),
//      /*params=*/{addressParamDecl},
//      /*RParenLoc=*/SourceLoc()
//  );
//
//  // Func name: init(from:)
//  DeclName name(C, DeclBaseName::createConstructor(), paramList);
//
//  auto *initDecl =
//      new (C) ConstructorDecl(name, SourceLoc(),
//                              /*Failable=*/false, SourceLoc(),
//                              /*Async=*/false, SourceLoc(),
//                              /*Throws=*/true, SourceLoc(),
//                              paramList,
//                              /*GenericParams=*/nullptr, decl);
//  initDecl->setImplicit();
//  initDecl->setSynthesized();
//  initDecl->setBodySynthesizer(&createDistributedActor_init_fromDecoder_body);
//
//  auto *nonIsoAttr = new (C) NonisolatedAttr(/*IsImplicit*/true);
//  initDecl->getAttrs().add(nonIsoAttr);
//
//  initDecl->copyFormalAccessFrom(decl, /*sourceIsParentContext=*/true);
//
//  return initDecl;
//}

// === Resolve -----------------------------------------------------------------

/// Synthesizes the
///
/// \verbatim
/// static resolve(_ address: ActorAddress,
///                using transport: ActorTransport) throws -> Self {
///   <filled in by SILGenDistributed>
/// }
/// \endverbatim
///
/// factory function in the AST, with an empty body. Its body is
/// expected to be filled-in during SILGen.
// TODO(distributed): move this synthesis to DerivedConformance style
static FuncDecl *deriveDistributedActor_resolve(DerivedConformance &derived) {
  auto decl = dyn_cast<ClassDecl>(derived.Nominal);
  assert(decl->isDistributedActor());
  auto &C = decl->getASTContext();

  auto mkParam = [&](Identifier argName, Identifier paramName, Type ty) -> ParamDecl* {
    auto *param = new (C) ParamDecl(SourceLoc(),
                                    SourceLoc(), argName,
                                    SourceLoc(), paramName, decl);
    param->setImplicit();
    param->setSpecifier(ParamSpecifier::Default);
    param->setInterfaceType(ty);
    return param;
  };

  auto addressType = getDistributedActorIdentityType(decl);
  auto transportType = getDistributedActorTransportType(decl);

  // (_ identity: Identity, using transport: ActorTransport)
  auto *params = ParameterList::create(
      C,
      /*LParenLoc=*/SourceLoc(),
      /*params=*/{  mkParam(Identifier(), C.Id_identity, addressType),
                  mkParam(C.Id_using, C.Id_transport, transportType)
      },
      /*RParenLoc=*/SourceLoc()
  );

  // Func name: resolve(_:using:)
  DeclName name(C, C.Id_resolve, params);

  // Expected type: (Self) -> (Identity, ActorTransport) throws -> (Self)
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
/******************************* PROPERTIES ***********************************/
/******************************************************************************/

static ValueDecl *deriveDistributedActor_id(DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // nonisolated
  // let id: Identity
  // ```
  auto propertyType = getDistributedActorIdentityType(derived.Nominal);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_id,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));


  // ====
  // If the Identity is Codable, the distributed actor is also Codable
  auto encodableProto = C.getProtocol(KnownProtocolKind::Encodable);
  auto decodableProto = C.getProtocol(KnownProtocolKind::Decodable);

  fprintf(stderr, "[%s:%d] (%s) DERIVE ID...\n", __FILE__, __LINE__, __FUNCTION__);

//  if (auto identityNominal = propertyType->getAnyNominal()) {
//  fprintf(stderr, "[%s:%d] (%s) IDENTITY IS...\n", __FILE__, __LINE__, __FUNCTION__);
//    identityNominal->dump();
//
//    auto conformances = identityNominal->getAllConformances();
//    if (identityNominal->lookupConformance(encodableProto, conformances)) {
//        fprintf(stderr, "[%s:%d] (%s) IDENTITY IS ENCODABLE...\n", __FILE__, __LINE__, __FUNCTION__);
//      // Identity is Encodable, synthesize the impl for this distributed actor
//      deriveDistributedActor_encode(derived);
//    }
//    if (identityNominal->lookupConformance(decodableProto, conformances)) {
//        fprintf(stderr, "[%s:%d] (%s) IDENTITY IS DECODABLE...\n", __FILE__, __LINE__, __FUNCTION__);
//      // Identity is Encodable, synthesize the impl for this distributed actor
//      deriveDistributedActor_init_fromDecoder(derived);
//    }
//  }

  derived.addMembersToConformanceContext({ propDecl, pbDecl });
  return propDecl;
}

static ValueDecl *deriveDistributedActor_actorTransport(
    DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // nonisolated
  // let actorTransport: Transport
  // ```
  // (no need for @actorIndependent because it is an immutable let)
  auto propertyType = getDistributedActorTransportType(derived.Nominal);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_actorTransport,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  derived.addMembersToConformanceContext({ propDecl, pbDecl });
  return propDecl;
}

static Type deriveDistributedActor_Transport(
    DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // Look for a type DefaultActorTransport within the parent context.
  auto defaultTransportLookup = TypeChecker::lookupUnqualified(
      derived.getConformanceContext()->getModuleScopeContext(),
      DeclNameRef(C.Id_DefaultActorTransport),
      derived.ConformanceDecl->getLoc());
  TypeDecl *defaultTransportTypeDecl = nullptr;
  for (const auto &found : defaultTransportLookup) {
    if (auto foundType = dyn_cast_or_null<TypeDecl>(found.getValueDecl())) {
      if (defaultTransportTypeDecl) {
        // Note: ambiguity, for now just fail.
        return nullptr;
      }

      defaultTransportTypeDecl = foundType;
      continue;
    }
  }

  // There is no default, so fail to synthesize.
  if (!defaultTransportTypeDecl)
    return nullptr;

  // Return the default transport type.
  return defaultTransportTypeDecl->getDeclaredInterfaceType();
}
// ==== ------------------------------------------------------------------------

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  if (auto var = dyn_cast<VarDecl>(requirement)) {
    if (var->getName() == Context.Id_id)
      return deriveDistributedActor_id(*this);

    if (var->getName() == Context.Id_actorTransport)
      return deriveDistributedActor_actorTransport(*this);
  }

  if (auto func = dyn_cast<FuncDecl>(requirement)) {
    // just a simple name check is enough here,
    // if we are invoked here we know for sure it is for the "right" function
    if (func->getName().getBaseName() == Context.Id_resolve)
      return deriveDistributedActor_resolve(*this);
  }

  return nullptr;
}

std::pair<Type, TypeDecl *> DerivedConformance::deriveDistributedActor(
    AssociatedTypeDecl *assocType) {
  if (!canDeriveDistributedActor(Nominal, cast<DeclContext>(ConformanceDecl)))
    return std::make_pair(Type(), nullptr);

  if (assocType->getName() == Context.Id_Transport) {
    return std::make_pair(deriveDistributedActor_Transport(*this), nullptr);
  }

  Context.Diags.diagnose(assocType->getLoc(),
                         diag::broken_distributed_actor_requirement);
  return std::make_pair(Type(), nullptr);
}
