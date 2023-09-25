//===--- TypeCheckInvertible.cpp -  Type checking invertible protocols ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for evaluating whether a type
// conforms to an invertible protocol. An invertible protocol is a known
// protocol KP for which the type ~KP exists.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckInvertible.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericEnvironment.h"
#include "TypeChecker.h"

namespace swift {

//static bool hasConformanceRequirement(ArrayRef<Requirement> reqs,
//                                      KnownProtocolKind query) {
//  for (auto req : reqs) {
//    if (req.getKind() == RequirementKind::Conformance)
//      if (auto kp = req.getSecondType()->getKnownProtocol())
//        if (*kp == query)
//          return true;
//  }
//  return false;
//}

//static const GenericContext *findGenericContext(DeclContext *dc) {
//  do {
//    if (auto decl = dc->getAsDecl())
//      if (auto GC = decl->getAsGenericContext())
//        return GC;
//  } while ((dc = dc->getParent()));
//
//  return nullptr;
//}

bool canBeNoncopyable(Type type) {
  auto &ctx = type->getASTContext();
  if (auto *nominal = type->getNominalOrBoundGenericNominal())
    return evaluateOrDefault(ctx.evaluator, IsNoncopyableRequest{nominal},
                             /*default=*/false);

  if (auto *expansion = type->getAs<PackExpansionType>()) {
    return canBeNoncopyable(expansion->getPatternType());
  }

  // if any components of the tuple are move-only, then the tuple is move-only.
  if (auto *tupl = type->getCanonicalType()->getAs<TupleType>()) {
    for (auto eltTy : tupl->getElementTypes())
      if (canBeNoncopyable(eltTy))
        return true;
  }

  return false; // otherwise, the conservative assumption is it's copyable.
}

bool isNoncopyable(Type type, DeclContext *dc) { return !isCopyable(type, dc); }

bool isCopyable(Type type, DeclContext *dc) {
  assert(dc);
  auto contextTy = dc->mapTypeIntoContext(type);
  auto *module = dc->getParentModule();
  return TypeChecker::conformsToKnownProtocol(contextTy,
                                              KnownProtocolKind::Copyable,
                                              module, false);
}

ProtocolConformance *deriveConformanceForInvertible(Evaluator &evaluator,
                                                    NominalTypeDecl *nominal,
                                                    KnownProtocolKind kp) {
  auto &ctx = nominal->getASTContext();
  auto *proto = ctx.getProtocol(kp);

  switch (kp) {
  case KnownProtocolKind::Copyable: {
    if (evaluateOrDefault(evaluator, IsNoncopyableRequest{nominal}, false))
      return nullptr; // it's not Copyable.

    // form a full unconditional conformance to Copyable.
    auto conformance = ctx.getNormalConformance(
        nominal->getDeclaredInterfaceType(), proto, nominal->getLoc(),
        nominal, ProtocolConformanceState::Complete,
        /*isUnchecked=*/false);
    conformance->setSourceKindAndImplyingConformance(
        ConformanceEntryKind::Synthesized, nullptr);

    nominal->registerProtocolConformance(conformance, /*synthesized=*/true);
    return conformance;

  }
  default:
    llvm_unreachable("not invertible");
  }
}

}
