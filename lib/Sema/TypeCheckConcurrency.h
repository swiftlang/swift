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

class ActorIsolation;
class ClassDecl;
class DeclContext;
class Expr;
class FuncDecl;
class ValueDecl;

/// Add notes suggesting the addition of 'async' or '@asyncHandler', as
/// appropriate, to a diagnostic for a function that isn't an async context.
void addAsyncNotes(FuncDecl *func);

/// Check actor isolation rules.
void checkActorIsolation(const Expr *expr, const DeclContext *dc);

/// Determine how the given value declaration is isolated.
ActorIsolation getActorIsolation(ValueDecl *value);

} // end namespace swift

#endif /* SWIFT_SEMA_TYPECHECKCONCURRENCY_H */
