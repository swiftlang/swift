//===--- AvailabilityInference.h - Swift Availability Utilities -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines utilities for computing declaration availability.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_INFERENCE_H
#define SWIFT_AST_AVAILABILITY_INFERENCE_H

#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/Type.h"
#include "llvm/Support/VersionTuple.h"
#include <optional>

namespace swift {
class ASTContext;
class AvailabilityDomain;
class BackDeployedAttr;
class Decl;
class SemanticAvailableAttr;

class AvailabilityInference {
public:
  /// Returns the decl that should be considered the parent decl of the given
  /// decl when looking for inherited availability annotations.
  static const Decl *parentDeclForInferredAvailability(const Decl *D);

  /// Infers the common availability required to access an array of
  /// declarations and adds attributes reflecting that availability
  /// to ToDecl.
  static void
  applyInferredAvailableAttrs(Decl *ToDecl,
                              ArrayRef<const Decl *> InferredFromDecls);

  static AvailabilityRange inferForType(Type t);

  /// Returns the range of platform versions in which the decl is available.
  static AvailabilityRange availableRange(const Decl *D);

  /// Returns true is the declaration is `@_spi_available`.
  static bool isAvailableAsSPI(const Decl *D);

  /// Returns the context for which the declaration
  /// is annotated as available, or None if the declaration
  /// has no availability annotation.
  static std::optional<AvailabilityRange>
  annotatedAvailableRange(const Decl *D);

  static AvailabilityRange
  annotatedAvailableRangeForAttr(const Decl *D, const SpecializeAttr *attr,
                                 ASTContext &ctx);

  /// For the attribute's introduction version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateIntroducedAvailabilityDomainForFallback(
      const SemanticAvailableAttr &attr, const ASTContext &ctx,
      AvailabilityDomain &domain, llvm::VersionTuple &platformVer);

  /// For the attribute's deprecation version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateDeprecatedAvailabilityDomainForFallback(
      const SemanticAvailableAttr &attr, const ASTContext &ctx,
      AvailabilityDomain &domain, llvm::VersionTuple &platformVer);

  /// For the attribute's obsoletion version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateObsoletedAvailabilityDomainForFallback(
      const SemanticAvailableAttr &attr, const ASTContext &ctx,
      AvailabilityDomain &domain, llvm::VersionTuple &platformVer);

  /// For the attribute's before version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateBeforeAvailabilityDomainForFallback(
      const BackDeployedAttr *attr, const ASTContext &ctx,
      AvailabilityDomain &domain, llvm::VersionTuple &platformVer);
};

} // end namespace swift

#endif
