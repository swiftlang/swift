//===--- Diagnostics.h - Requirement machine diagnostics --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REQUIREMENT_DIAGNOSTICS_H
#define SWIFT_REQUIREMENT_DIAGNOSTICS_H

#include "swift/AST/Requirement.h"
#include "swift/AST/Type.h"

namespace swift {

namespace rewriting {

/// Represents an invalid requirement, such as `T: Int`.
///
/// Invalid requirements are recorded while computing the
/// generic signature of a declaration, and diagnosed via
/// \c diagnoseRequirementErrors .
struct RequirementError {
  /// The kind of requirement error.
  enum class Kind {
    /// A constraint to a non-protocol, non-class type, e.g. T: Int.
    InvalidTypeRequirement,
    /// A type mismatch, e.g. Int == String.
    ConcreteTypeMismatch,
    /// A requirement proven to be false, e.g. Bool: Collection
    ConflictingRequirement,
    /// A redundant requirement, e.g. T == T.
    RedundantRequirement,
  } kind;

  /// The invalid requirement.
  Requirement requirement;

  SourceLoc loc;

private:
  RequirementError(Kind kind, Requirement requirement, SourceLoc loc)
    : kind(kind), requirement(requirement), loc(loc) {}

public:
  static RequirementError forInvalidTypeRequirement(Type subjectType,
                                                    Type constraint,
                                                    SourceLoc loc) {
    Requirement requirement(RequirementKind::Conformance, subjectType, constraint);
    return {Kind::InvalidTypeRequirement, requirement, loc};
  }

  static RequirementError forConcreteTypeMismatch(Type type1,
                                                  Type type2,
                                                  SourceLoc loc) {
    Requirement requirement(RequirementKind::SameType, type1, type2);
    return {Kind::ConcreteTypeMismatch, requirement, loc};
  }

  static RequirementError forConflictingRequirement(Requirement req,
                                                    SourceLoc loc) {
    return {Kind::ConflictingRequirement, req, loc};
  }

  static RequirementError forRedundantRequirement(Requirement req,
                                                  SourceLoc loc) {
    return {Kind::RedundantRequirement, req, loc};
  }
};

} // end namespace rewriting

} // end namespace swift

#endif
