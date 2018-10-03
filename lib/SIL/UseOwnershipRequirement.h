//===--- UseOwnershipRequirement.h ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file contains declarations that enable clients to compute the ownership
/// requirements that a use puts on an SSA value. This is used in coordination
/// with the OwnershipChecker in SILOwnershipVerifier.cpp and the SILValue
/// ValueOwnershipKind visitor in ValueOwnershipKindClassifier.cpp to perform
/// SIL level ownership verification.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_USEOWNERSHIPREQUIREMENT_H
#define SWIFT_SIL_USEOWNERSHIPREQUIREMENT_H

namespace swift {

/// What constraint does the given use of an SSA value put on the lifetime of
/// the given SSA value.
///
/// There are two possible constraints: MustBeLive and
/// MustBeInvalidated. MustBeLive means that the SSA value must be able to be
/// used in a valid way at the given use point. MustBeInvalidated means that any
/// use of given SSA value after this instruction on any path through this
/// instruction.
enum class UseLifetimeConstraint {
  /// This use requires the SSA value to be live after the given instruction's
  /// execution.
  MustBeLive,

  /// This use invalidates the given SSA value.
  ///
  /// This means that the given SSA value can not have any uses that are
  /// reachable from this instruction. When a value has owned semantics this
  /// means the SSA value is destroyed at this point. When a value has
  /// guaranteed (i.e. shared borrow) semantics this means that the program
  /// has left the scope of the borrowed SSA value and said value can not be
  /// used.
  MustBeInvalidated,
};

} // end namespace swift

#endif
