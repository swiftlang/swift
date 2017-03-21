//===--- ArgumentScope.h --------------------------------------------------===//
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
  SILGenFunction &SGF;
  Scope normalScope;
  FormalEvaluationScope formalEvalScope;
  SILLocation loc;

public:
  ArgumentScope(SILGenFunction &SGF, SILLocation loc)
      : SGF(SGF), normalScope(SGF.Cleanups, CleanupLocation::get(loc)),
        formalEvalScope(SGF), loc(loc) {}

  ~ArgumentScope() {
    if (normalScope.isValid() || !formalEvalScope.isPopped()) {
      llvm_unreachable("ArgumentScope that wasn't popped?!");
    }
  }

  ArgumentScope() = delete;
  ArgumentScope(const ArgumentScope &) = delete;
  ArgumentScope &operator=(const ArgumentScope &) = delete;

  ArgumentScope(ArgumentScope &&other)
      : SGF(other.SGF), normalScope(std::move(other.normalScope)),
        formalEvalScope(std::move(other.formalEvalScope)), loc(other.loc) {}

  /// Deleted move assignment operator.
  ///
  /// This is done on purpose, since we only want to be able to move
  /// ArgumentScope into utility functions.
  ArgumentScope &operator=(ArgumentScope &&other) = delete;

  void pop() { popImpl(); }

  /// Pop the formal evaluation and argument scopes preserving the value mv.
  ///
  /// *NOTE* If mv is an address, it is assumed that one of the scopes will
  /// cause a dealloc stack to be emitted for mv and that the alloc_stack is
  /// within our scope. This means that we are essentially creating a lifetime
  /// extension of this value.
  ManagedValue popPreservingValue(ManagedValue mv) &&;

private:
  void popImpl() {
    // We must always pop the formal eval scope before the normal scope since
    // the formal eval scope may have pointers into the normal scope.
    formalEvalScope.pop();
    normalScope.pop();
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
