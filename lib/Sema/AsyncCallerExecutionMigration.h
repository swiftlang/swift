//===-- Sema/AsyncCallerExecutionMigration.h --------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file provides code migration support for the `AsyncCallerExecution`
/// feature.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_ASYNCCALLEREXECUTIONMIGRATION_H
#define SWIFT_SEMA_ASYNCCALLEREXECUTIONMIGRATION_H

#include "swift/AST/ActorIsolation.h"
#include "swift/AST/ExtInfo.h"

namespace swift {

class FunctionTypeRepr;
class ValueDecl;
class AbstractClosureExpr;

/// Warns that the behavior of nonisolated async functions will change under
/// `AsyncCallerExecution` and suggests `@execution(concurrent)` to preserve
/// the current behavior.
void warnAboutNewNonisolatedAsyncExecutionBehavior(
    ASTContext &ctx, FunctionTypeRepr *node, FunctionTypeIsolation isolation);

/// Warns that the behavior of nonisolated async functions will change under
/// `AsyncCallerExecution` and suggests `@execution(concurrent)` to preserve
/// the current behavior.
void warnAboutNewNonisolatedAsyncExecutionBehavior(ASTContext &ctx,
                                                   ValueDecl *node,
                                                   ActorIsolation isolation);

/// Warns that the behavior of nonisolated async functions will change under
/// `AsyncCallerExecution` and suggests `@execution(concurrent)` to preserve
/// the current behavior.
void warnAboutNewNonisolatedAsyncExecutionBehavior(ASTContext &ctx,
                                                   AbstractClosureExpr *node,
                                                   ActorIsolation isolation);

} // end namespace swift

#endif /* SWIFT_SEMA_ASYNCCALLEREXECUTIONMIGRATION_H */
