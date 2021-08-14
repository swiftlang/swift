//===--- ExecutorBreadcrumb.h - executor hop tracking for SILGen ----------===//
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

#include "swift/SIL/SILValue.h"

namespace swift {

class SILLocation;

namespace Lowering {

class SILGenFunction;

/// Represents the information necessary to return to a caller's own
/// active executor after making a hop to an actor for actor-isolated calls.
class ExecutorBreadcrumb {
  SILValue Executor;
  
public:
  // An empty breadcrumb, indicating no hop back is necessary.
  ExecutorBreadcrumb() : Executor() {}
  
  // A breadcrumb representing the need to hop back to the executor
  // represented by the given value.
  explicit ExecutorBreadcrumb(SILValue executor)
    : Executor(executor) {}
  
  // Emits the hop back sequence, if any, necessary to get back to
  // the executor represented by this breadcrumb.
  void emit(SILGenFunction &SGF, SILLocation loc);
};

} // namespace Lowering
} // namespace swift
