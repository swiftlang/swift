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
class AvailableAttr;
class BackDeployedAttr;
class Decl;

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

  /// Returns the range of platform versions in which the decl is available and
  /// the attribute which determined this range (which may be `nullptr` if the
  /// declaration is always available.
  static std::pair<AvailabilityRange, const AvailableAttr *>
  availableRangeAndAttr(const Decl *D);

  /// Returns true is the declaration is `@_spi_available`.
  static bool isAvailableAsSPI(const Decl *D);

  /// Returns the range of platform versions in which a declaration with the
  /// given `@available` attribute is available.
  ///
  /// NOTE: The attribute must be active on the current platform.
  static AvailabilityRange availableRange(const AvailableAttr *attr,
                                          ASTContext &C);

  /// Returns the attribute that should be used to determine the availability
  /// range of the given declaration, or nullptr if there is none.
  static const AvailableAttr *attrForAnnotatedAvailableRange(const Decl *D);

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
  static bool updateIntroducedPlatformForFallback(
      const AvailableAttr *attr, const ASTContext &Ctx,
      llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer);

  /// For the attribute's deprecation version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateDeprecatedPlatformForFallback(
      const AvailableAttr *attr, const ASTContext &Ctx,
      llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer);

  /// For the attribute's obsoletion version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateObsoletedPlatformForFallback(
      const AvailableAttr *attr, const ASTContext &Ctx,
      llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer);

  static void updatePlatformStringForFallback(
      const AvailableAttr *attr, const ASTContext &Ctx,
      llvm::StringRef &Platform);

  /// For the attribute's before version, update the platform and version
  /// values to the re-mapped platform's, if using a fallback platform.
  /// Returns `true` if a remap occured.
  static bool updateBeforePlatformForFallback(const BackDeployedAttr *attr,
                                              const ASTContext &Ctx,
                                              llvm::StringRef &Platform,
                                              llvm::VersionTuple &PlatformVer);
};

// FIXME: This should become a utility on Decl.

/// Given a declaration upon which an availability attribute would appear in
/// concrete syntax, return a declaration to which the parser
/// actually attaches the attribute in the abstract syntax tree. We use this
/// function to determine whether the concrete syntax already has an
/// availability attribute.
const Decl *abstractSyntaxDeclForAvailableAttribute(const Decl *D);

} // end namespace swift

#endif
