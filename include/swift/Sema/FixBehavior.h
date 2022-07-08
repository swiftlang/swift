//===--- FixBehavior.h - Constraint Fix Behavior --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides information about how a constraint fix should behavior.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_FIXBEHAVIOR_H
#define SWIFT_SEMA_FIXBEHAVIOR_H

namespace swift {
namespace constraints {

/// Describes the behavior of the diagnostic corresponding to a given fix.
enum class FixBehavior {
  /// The diagnostic is an error, and should prevent constraint application.
  Error,
  /// The diagnostic is always a warning, which should not prevent constraint
  /// application.
  AlwaysWarning,
  /// The diagnostic should be downgraded to a warning, and not prevent
  /// constraint application.
  DowngradeToWarning,
  /// The diagnostic should be suppressed, and not prevent constraint
  /// application.
  Suppress
};

}
}

#endif // SWIFT_SEMA_FIXBEHAVIOR_H
