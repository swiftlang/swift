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
    /// A type requirement on a trivially invalid subject type,
    /// e.g. Bool: Collection.
    InvalidRequirementSubject,
    /// A pair of conflicting requirements, T == Int, T == String
    ConflictingRequirement,
    /// A redundant requirement, e.g. T == T.
    RedundantRequirement,
  } kind;

  /// The type parameter on which there is an invalid or conflicting
  /// requirement.
  ///
  /// FIXME: We probably want to just store two separate requirements
  /// in the case of a confict. Right now, the conflicting constraint
  /// types are both stored in the requirement below, and this serves
  /// as the subject type.
  Type typeParameter;

  /// The invalid requirement.
  Requirement requirement;

  SourceLoc loc;

private:
  RequirementError(Kind kind, Requirement requirement, SourceLoc loc)
    : kind(kind), typeParameter(Type()), requirement(requirement), loc(loc) {}

  RequirementError(Kind kind, Type subject, Requirement requirement, SourceLoc loc)
    : kind(kind), typeParameter(subject), requirement(requirement), loc(loc) {}

public:
  static RequirementError forInvalidTypeRequirement(Type subjectType,
                                                    Type constraint,
                                                    SourceLoc loc) {
    Requirement requirement(RequirementKind::Conformance, subjectType, constraint);
    return {Kind::InvalidTypeRequirement, requirement, loc};
  }

  static RequirementError forInvalidRequirementSubject(Requirement req,
                                                       SourceLoc loc) {
    return {Kind::InvalidRequirementSubject, req, loc};
  }

  static RequirementError forConflictingRequirement(Requirement req,
                                                    SourceLoc loc) {
    return {Kind::ConflictingRequirement, req, loc};
  }

  static RequirementError forConflictingRequirement(Type typeParameter,
                                                    Requirement req,
                                                    SourceLoc loc) {
    return {Kind::ConflictingRequirement, typeParameter, req, loc};
  }

  static RequirementError forRedundantRequirement(Requirement req,
                                                  SourceLoc loc) {
    return {Kind::RedundantRequirement, req, loc};
  }
};

} // end namespace rewriting

} // end namespace swift

#endif
