//===--- ArgumentScope.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_ARGUMENTSCOPE_H
#define SWIFT_SILGEN_ARGUMENTSCOPE_H

#include "FormalEvaluation.h"
#include "SILGenFunction.h"
#include "Scope.h"

namespace swift {
namespace Lowering {

/// A scope that is created before arguments are emitted for an apply and is
/// popped manually immediately after the raw apply is emitted and before the
/// raw results are marshalled into managed resources.
///
/// Internally this creates a Scope and a FormalEvaluationScope. It ensures that
/// they are destroyed/initialized in the appropriate order.
class ArgumentScope {
  Scope normalScope;
  FormalEvaluationScope formalEvalScope;
  SILLocation loc;

public:
  ArgumentScope(SILGenFunction &SGF, SILLocation loc)
      : normalScope(SGF.Cleanups, CleanupLocation::get(loc)),
        formalEvalScope(SGF), loc(loc) {}

  ~ArgumentScope() {
    if (normalScope.isValid() || !formalEvalScope.isPopped()) {
      llvm_unreachable("ArgumentScope that wasn't popped?!");
    }
  }

  ArgumentScope() = delete;
  ArgumentScope(const ArgumentScope &) = delete;
  ArgumentScope &operator=(const ArgumentScope &) = delete;

  ArgumentScope(ArgumentScope &&other) = delete;
  ArgumentScope &operator=(ArgumentScope &&other) = delete;

  void pop() { popImpl(); }

  /// Pop the formal evaluation and argument scopes preserving the value mv.
  ManagedValue popPreservingValue(ManagedValue mv);

  // Pop the formal evaluation and argument scopes, preserving rv.
  RValue popPreservingValue(RValue &&rv);

  void verify() {
    formalEvalScope.verify();
  }

private:
  void popImpl() {
    // We must always pop the formal eval scope before the normal scope since
    // the formal eval scope may have pointers into the normal scope.
    normalScope.verify();
    formalEvalScope.pop();
    normalScope.pop();
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
