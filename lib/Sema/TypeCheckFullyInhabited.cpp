//===------ TypeCheckFullyInhabited.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Semantic analysis for ConvertibleToBytes and ConvertibleFromBytes.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckFullyInhabited.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

static void assertProtocolIdentity(ProtocolConformance *conformance,
                                   KnownProtocolKind kind) {
  ASSERT(conformance->getProtocol() ==
         conformance->getDeclContext()
             ->getParentModule()
             ->getASTContext()
             .getProtocol(kind));
}

static void convertibleToBytesConformance(ProtocolConformance *conformance) {
  auto conformanceDC = conformance->getDeclContext();
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal)
    return;

  // If this is an always-unavailable conformance, there's nothing to check.
  if (auto ext = dyn_cast<ExtensionDecl>(conformanceDC)) {
    if (ext->isUnavailable())
      return;
  }

  // ConvertibleToBytes must be added in the same module or its overlay.
  auto conformanceDecl = conformanceDC->getAsDecl();
  if (!conformanceDecl->getModuleContext()->isSameModuleLookingThroughOverlays(
          nominal->getModuleContext())) {
    conformanceDecl->diagnose(diag::convertible_to_bytes_outside_module, nominal);
    return;
  }

  // Insert structural checking here:
  // - all stored properties must also be `ConvertibleToBytes`.
  // - there must be no padding.

  // ConvertibleToBytes must (currently) be a conformance in the standard library.
  if (conformanceDecl->getDeclContext()->getParentModule()->isStdlibModule()) {
    return;
  }
  conformanceDecl->diagnose(diag::convertible_to_bytes_outside_stdlib, nominal);
}

void swift::checkConvertibleToBytesConformance(ProtocolConformance *conformance) {
  assertProtocolIdentity(conformance, KnownProtocolKind::ConvertibleToBytes);
  convertibleToBytesConformance(conformance);
}

static void convertibleFromBytesConformance(ProtocolConformance *conformance) {
  auto conformanceDC = conformance->getDeclContext();
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal)
    return;

  // If this is an always-unavailable conformance, there's nothing to check.
  if (auto ext = dyn_cast<ExtensionDecl>(conformanceDC)) {
    if (ext->isUnavailable())
      return;
  }

  // ConvertibleFromBytes must be added in the same module or its overlay.
  auto conformanceDecl = conformanceDC->getAsDecl();
  if (!conformanceDecl->getModuleContext()->isSameModuleLookingThroughOverlays(
          nominal->getModuleContext())) {
    conformanceDecl->diagnose(diag::convertible_from_bytes_outside_module, nominal);
    return;
  }

  // Insert structural checking here:
  // - all stored properties must also be `ConvertibleFromBytes`.

  // ConvertibleFromBytes is a valid conformance in the standard library.
  if (conformanceDecl->getDeclContext()->getParentModule()->isStdlibModule()) {
    return;
  }

  // Allow conformances outside the stdlib with @unchecked.
  // if (auto *normal = conformance->getRootNormalConformance()) {
  //   if (normal->isUnchecked()) {
  //     return;
  //   }
  // }
  conformanceDecl->diagnose(diag::convertible_from_bytes_outside_stdlib, nominal);
}

void swift::checkConvertibleFromBytesConformance(ProtocolConformance *conformance) {
  assertProtocolIdentity(conformance, KnownProtocolKind::ConvertibleFromBytes);
  convertibleFromBytesConformance(conformance);
}
