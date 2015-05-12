//===--- Availability.h - Swift Availability Structures -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures for API availability.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_H
#define SWIFT_AST_AVAILABILITY_H

#include "swift/Basic/LLVM.h"
#include "clang/Basic/VersionTuple.h"
#include "llvm/ADT/Optional.h"

namespace swift {
class ASTContext;
class Decl;

/// A lattice of version ranges of the form [x.y.z, +Inf).
class VersionRange {
  // The lattice ordering is linear:
  // Empty <= ... <= [10.10.0,+Inf) <= ... [10.1.0,+Inf) <=  ... <= All
  // and corresponds to set inclusion.

  // The concretization of lattice elements is:
  //    Empty: empty
  //    All: all versions
  //    x.y.x: all versions greater than or equal to to x.y.z

  enum class ExtremalRange { Empty, All };
  
  // A version range is either an extremal value (Empty, All) or
  // a single version tuple value representing the lower end point x.y.z of a
  // range [x.y.z, +Inf).
  union {
    clang::VersionTuple LowerEndpoint;
    ExtremalRange ExtremalValue;
  };
  
  unsigned HasLowerEndpoint : 1;

public:
  /// Returns true if the range of versions is empty, or false otherwise.
  bool isEmpty() const {
    return !HasLowerEndpoint && ExtremalValue == ExtremalRange::Empty;
  }

  /// Returns true if the range includes all versions, or false otherwise.
  bool isAll() const {
    return !HasLowerEndpoint && ExtremalValue == ExtremalRange::All;
  }

  /// Returns true if the range has a lower end point; that is, if it is of
  /// the form [X, +Inf).
  bool hasLowerEndpoint() const { return HasLowerEndpoint; }

  /// Returns the range's lower endpoint.
  const clang::VersionTuple &getLowerEndpoint() const {
    assert(HasLowerEndpoint);
    return LowerEndpoint;
  }

  /// Returns a representation of this range as a string for debugging purposes.
  std::string getAsString() const {
    if (isEmpty()) {
      return "empty";
    } else if (isAll()) {
      return "all";
    } else {
      return "[" + getLowerEndpoint().getAsString() + ",+Inf)";
    }
  }

  /// Returns true if all versions in this range are also in the Other range.
  bool isContainedIn(const VersionRange &Other) const {
    if (isEmpty() || Other.isAll())
      return true;
    
    if (isAll() || Other.isEmpty())
      return false;

    // [v1, +Inf) is contained in [v2, +Inf) if v1 >= v2
    return getLowerEndpoint() >= Other.getLowerEndpoint();
  }

  /// Mutates this range to be the greatest lower bound of itself and Other.
  void meetWith(const VersionRange &Other) {
    if (isEmpty() || Other.isAll())
      return;

    if (isAll() || Other.isEmpty()) {
      *this = Other;
      return;
    }

    // The g.l.b of [v1, +Inf), [v2, +Inf) is [max(v1,v2), +Inf)
    const clang::VersionTuple maxVersion =
        std::max(this->getLowerEndpoint(), Other.getLowerEndpoint());

    setLowerEndpoint(maxVersion);
  }

  /// Mutates this range to be a best effort over-approximation of the
  /// intersection of the concretizations of this version range and Other.
  void constrainWith(const VersionRange &Other) {
    // We can use the meet for this because the lattice is multiplicative
    // with respect to concretization--that is, the concretization
    // of Range1 meet Range2 is equal to the intersection of the
    // concretization of Range1 and the concretization of Range2.
    // This will change if we add (-Inf, v) to our version range lattice.
    meetWith(Other);
  }

  /// Returns a version range representing all versions.
  static VersionRange all() { return VersionRange(ExtremalRange::All); }

  /// Returns a version range representing no versions.
  static VersionRange empty() { return VersionRange(ExtremalRange::Empty); }

  /// Returns a version range representing all versions greater than or equal
  /// to the passed-in version.
  static VersionRange allGTE(const clang::VersionTuple &EndPoint) {
    return VersionRange(EndPoint);
  }

private:
  VersionRange(const clang::VersionTuple &LowerEndpoint) {
    setLowerEndpoint(LowerEndpoint);
  }

  VersionRange(ExtremalRange ExtremalValue) {
    setExtremalRange(ExtremalValue);
  }

  void setExtremalRange(ExtremalRange Version) {
    HasLowerEndpoint = 0;
    ExtremalValue = Version;
  }

  void setLowerEndpoint(const clang::VersionTuple &Version) {
    HasLowerEndpoint = 1;
    LowerEndpoint = Version;
  }
};

/// Records the reason a declaration is potentially unavailable.
class UnavailabilityReason {
public:
  enum class Kind {
    /// The declaration is potentially unavailable because it requires an OS
    /// version range that is not guaranteed by the minimum deployment
    /// target.
    RequiresOSVersionRange,

    /// The declaration is potentially unavailable because it is explicitly
    /// weakly linked.
    ExplicitlyWeakLinked
  };

private:
  // A value of None indicates the declaration is potentially unavailable
  // because it is explicitly weak linked.
  Optional<VersionRange> RequiredDeploymentRange;

  UnavailabilityReason(const Optional<VersionRange> &RequiredDeploymentRange)
      : RequiredDeploymentRange(RequiredDeploymentRange) {}

public:
  static UnavailabilityReason explicitlyWeaklyLinked() {
    return UnavailabilityReason(None);
  }

  static UnavailabilityReason requiresVersionRange(const VersionRange Range) {
    return UnavailabilityReason(Range);
  }

  Kind getReasonKind() const {
    if (RequiredDeploymentRange.hasValue()) {
      return Kind::RequiresOSVersionRange;
    } else {
      return Kind::ExplicitlyWeakLinked;
    }
  }

  const VersionRange &getRequiredOSVersionRange() const {
    assert(getReasonKind() == Kind::RequiresOSVersionRange);
    return RequiredDeploymentRange.getValue();
  }
};


class AvailabilityInference {
public:
  /// Infers the common availability required to access an array of
  /// declarations and adds attributes reflecting that availability
  /// to ToDecl.
  static void
  applyInferredAvailableAttrs(Decl *ToDecl,
                                 ArrayRef<const Decl *> InferredFromDecls,
                                 ASTContext &Context);
};

} // end namespace swift

#endif
