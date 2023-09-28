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


/// Visit the instance storage of the given nominal type as seen through
/// the given declaration context.
bool StorageVisitor::visit(NominalTypeDecl *nominal, DeclContext *dc) {
  // Walk the stored properties of classes and structs.
  if (isa<StructDecl>(nominal) || isa<ClassDecl>(nominal)) {
    for (auto property : nominal->getStoredProperties()) {
      auto propertyType = dc->mapTypeIntoContext(property->getInterfaceType())
          ->getRValueType()->getReferenceStorageReferent();
      if ((*this)(property, propertyType))
        return true;
    }

    return false;
  }

  // Walk the enum elements that have associated values.
  if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
    for (auto caseDecl : enumDecl->getAllCases()) {
      for (auto element : caseDecl->getElements()) {
        if (!element->hasAssociatedValues())
          continue;

        // Check that the associated value type is Sendable.
        auto elementType = dc->mapTypeIntoContext(
            element->getArgumentInterfaceType());
        if ((*this)(element, elementType))
          return true;
      }
    }

    return false;
  }

  return false;
}

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

/// \returns true iff the given nominal meets the requirements of Copyable>
bool checkCopyableConformance(Evaluator &evaluator,
                              NominalTypeDecl *nom) {
  // All classes can store noncopyable values.
  if (isa<ClassDecl>(nom))
    return true;

  // Protocols do not directly define any storage.
  if (isa<ProtocolDecl>(nom))
    return true;

  if (isa<BuiltinTupleDecl>(nom))
    llvm_unreachable("unexpected BuiltinTupleDecl");

  if (auto strct = dyn_cast<StructDecl>(nom)) {
    for (auto stored : strct->getStoredProperties()) {
//      stored->getInterfaceType()
//      ImplicitKnownProtocolConformanceRequest
    }
  }

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

    // Otherwise, verify Copyable conformance.



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
