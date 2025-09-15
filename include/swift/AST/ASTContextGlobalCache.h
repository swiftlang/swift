//===--- ASTContextGlobalCache.h - AST Context Cache ------------*- C++ -*-===//
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
// This file defines the ASTContext::GlobalCache type. DO NOT include this
// header from any other header: it should only be included in those .cpp
// files that need to access the side tables. There are no include guards to
// force the issue.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ActorIsolation.h"
#include <variant>

namespace swift {

class NormalProtocolConformance;
class ValueDecl;

/// Describes an isolation error where the witness has actor isolation that
/// conflicts with the requirement's expectation.
struct WitnessIsolationError {
  /// The requirement declaration.
  ValueDecl *requirement;

  /// The witness.
  ValueDecl *witness;

  /// The pre-Swift-6 diagnostic behavior.
  DiagnosticBehavior behavior;

  /// Whether the witness is missing "distributed".
  bool isMissingDistributed;

  /// The isolation of the requirement, mapped into the conformance.
  ActorIsolation requirementIsolation;

  /// The isolation domain that the witness would need to go into for it
  /// to be suitable for the requirement.
  ActorIsolation referenceIsolation;

  /// Diagnose this witness isolation error.
  void diagnose(const NormalProtocolConformance *conformance) const;
};

/// Describes an isolation error involving an associated conformance.
struct AssociatedConformanceIsolationError {
  ProtocolConformance *isolatedConformance;
  DiagnosticBehavior behavior = DiagnosticBehavior::Unspecified;

  /// Diagnose this associated conformance isolation error.
  void diagnose(const NormalProtocolConformance *conformance) const;
};

/// Describes an isolation error that occurred during conformance checking.
using ConformanceIsolationError = std::variant<
    WitnessIsolationError, AssociatedConformanceIsolationError>;

/// A collection of side tables associated with the ASTContext itself, meant
/// as a way to keep sparse information out of the AST nodes themselves and
/// not require one to touch ASTContext.h to add such information.
struct ASTContext::GlobalCache {
  /// Mapping from normal protocol conformances to the explicitly-specified
  /// global actor isolations, e.g., when the conformance was spelled
  /// `@MainActor P` or similar.
  llvm::DenseMap<const NormalProtocolConformance *, TypeExpr *>
      conformanceExplicitGlobalActorIsolation;

  /// Mapping from normal protocol conformances to the set of actor isolation
  /// problems that occur within the conformances.
  ///
  /// This map will be empty for well-formed code, and is used to accumulate
  /// information so that the diagnostics can be coalesced.
  llvm::DenseMap<
    const NormalProtocolConformance *,
    std::vector<ConformanceIsolationError>
  > conformanceIsolationErrors;
};

} // end namespace 
