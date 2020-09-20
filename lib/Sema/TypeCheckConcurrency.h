//===--- TypeCheckConcurrency.h - Concurrency -------------------*- C++ -*-===//
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
// This file provides type checking support for Swift's concurrency model.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKCONCURRENCY_H
#define SWIFT_SEMA_TYPECHECKCONCURRENCY_H

namespace swift {

class ClassDecl;
class DeclContext;
class Expr;
class FuncDecl;
class ValueDecl;

/// Check whether the @asyncHandler attribute can be applied to the given
/// function declaration.
///
/// \param diagnose Whether to emit a diagnostic when a problem is encountered.
///
/// \returns \c true if there was a problem with adding the attribute, \c false
/// otherwise.
bool checkAsyncHandler(FuncDecl *func, bool diagnose);

/// Add notes suggesting the addition of 'async' or '@asyncHandler', as
/// appropriate, to a diagnostic for a function that isn't an async context.
void addAsyncNotes(FuncDecl *func);

/// Check actor isolation rules.
void checkActorIsolation(const Expr *expr, const DeclContext *dc);

/// Determine whether the given value is an instance member of an actor
/// class that is isolated to the current actor instance.
///
/// \returns the type of the actor.
ClassDecl *getActorIsolatingMember(ValueDecl *value);

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKCONCURRENCY_H */
