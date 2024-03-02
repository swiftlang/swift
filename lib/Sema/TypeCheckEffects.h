//===--- TypeCheckEffects.h - Effects checking ------------------*- C++ -*-===//
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
// This file provides type checking support for Swift's effect checking, which
// includes error handling (throws) and asynchronous (async) effects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKEFFECTS_H
#define SWIFT_SEMA_TYPECHECKEFFECTS_H

#include "swift/AST/Type.h"

namespace swift {

/// Classifies the result of a subtyping comparison between two thrown error
/// types.
enum class ThrownErrorSubtyping {
  /// There is no subtyping relationship, and we're trying to convert from a
  /// throwning type to a non-throwing type.
  DropsThrows,
  /// There is no subtyping relationship because the types mismatch.
  Mismatch,
  /// The thrown error types exactly match; there is a subtype relationship.
  ExactMatch,
  /// The thrown error types are different, but there is an obvious subtype
  /// relationship.
  Subtype,
  /// The thrown error types are different, and the presence of type variables
  /// or type parameters prevents us from determining now whether there is a
  /// subtype relationship.
  Dependent,
};

/// Compare the thrown error types for the purposes of subtyping.
ThrownErrorSubtyping compareThrownErrorsForSubtyping(
    Type subThrownError, Type superThrownError, DeclContext *dc);

/// Determine whether the given function uses typed throws in a manner
/// that is structurally similar to 'rethrows', e.g.,
///
/// \code
/// func map<T, E>(_ body: (Element) throws(E) -> T) throws(E) -> [T]
/// \endcode
bool isRethrowLikeTypedThrows(AbstractFunctionDecl *func);

}

#endif // SWIFT_SEMA_TYPECHECKEFFECTS_H
