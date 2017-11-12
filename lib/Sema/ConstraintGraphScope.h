//===--- ConstraintGraphScope.h - Constraint Graph Scope --------*- C++ -*-===//
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
//
// This file defines the \c ConstraintGraphScope class, an RAII object that
// introduces a new scope in which changes to the constraint graph are
// capture and will be reverted when the scope disappears.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONSTRAINT_GRAPH_SCOPE_H
#define SWIFT_SEMA_CONSTRAINT_GRAPH_SCOPE_H

namespace swift {
namespace constraints {

class ConstraintGraph;

/// RAII object that introduces a new constraint graph scope to capture
/// modifications made to the constraint graph.
///
/// Scopes are arranged in a stack, where the active scope is the top of the
/// stack. All changes made to the constraint graph are recorded in the
/// active scope. When the scope is popped (via the RAII object destructor),
/// those changes are reverted.
class ConstraintGraphScope {
  ConstraintGraph &CG;

  /// The parent scope, or null if this is the topmost scope.
  ConstraintGraphScope *ParentScope;

  /// The number of recorded changes that existed at the time this scope was
  /// formed.
  ///
  /// When this scope exits, any changes beyond this will be reverted to
  /// bring the graph back to its state prior to the introduction of this
  /// variable.
  unsigned NumChanges;

  ConstraintGraphScope(const ConstraintGraphScope&) = delete;
  ConstraintGraphScope &operator=(const ConstraintGraphScope&) = delete;

public:
  explicit ConstraintGraphScope(ConstraintGraph &CG);
  ~ConstraintGraphScope();
};

} // end namespace constraints
} // end namespace swift

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_GRAPH_SCOPE_H
