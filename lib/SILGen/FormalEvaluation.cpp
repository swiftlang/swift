//===--- FormalEvaluation.cpp ---------------------------------------------===//
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

#include "FormalEvaluation.h"
#include "LValue.h"
#include "SILGenFunction.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                               Formal Access
//===----------------------------------------------------------------------===//

void FormalAccess::_anchor() {}

//===----------------------------------------------------------------------===//
//                      Shared Borrow Formal Evaluation
//===----------------------------------------------------------------------===//

void SharedBorrowFormalAccess::finishImpl(SILGenFunction &SGF) {
  SGF.B.createEndBorrow(CleanupLocation::get(loc), borrowedValue,
                        originalValue);
}

//===----------------------------------------------------------------------===//
//                             OwnedFormalAccess
//===----------------------------------------------------------------------===//

void OwnedFormalAccess::finishImpl(SILGenFunction &SGF) {
  auto cleanupLoc = CleanupLocation::get(loc);
  if (value->getType().isAddress())
    SGF.B.createDestroyAddr(cleanupLoc, value);
  else
    SGF.B.emitDestroyValueOperation(cleanupLoc, value);
}

//===----------------------------------------------------------------------===//
//                          Formal Evaluation Scope
//===----------------------------------------------------------------------===//

FormalEvaluationScope::FormalEvaluationScope(SILGenFunction &SGF)
    : SGF(SGF), savedDepth(SGF.FormalEvalContext.stable_begin()),
      wasInWritebackScope(SGF.InWritebackScope),
      wasInInOutConversionScope(SGF.InInOutConversionScope) {
  if (wasInInOutConversionScope) {
    savedDepth.reset();
    return;
  }
  SGF.InWritebackScope = true;
}

FormalEvaluationScope::FormalEvaluationScope(FormalEvaluationScope &&o)
    : SGF(o.SGF), savedDepth(o.savedDepth),
      wasInWritebackScope(o.wasInWritebackScope),
      wasInInOutConversionScope(o.wasInInOutConversionScope) {
  o.savedDepth.reset();
}

void FormalEvaluationScope::popImpl() {
  // Pop the InWritebackScope bit.
  SGF.InWritebackScope = wasInWritebackScope;

  // Check to see if there is anything going on here.

  auto &context = SGF.FormalEvalContext;
  using iterator = FormalEvaluationContext::iterator;
  using stable_iterator = FormalEvaluationContext::stable_iterator;

  iterator unwrappedSavedDepth = context.find(savedDepth.getValue());
  iterator iter = context.begin();
  if (iter == unwrappedSavedDepth)
    return;

  // Save our start point to make sure that we are not adding any new cleanups
  // to the front of the stack.
  stable_iterator originalBegin = context.stable_begin();

  // Then working down the stack until we visit unwrappedSavedDepth...
  for (; iter != unwrappedSavedDepth; ++iter) {
    // Grab the next evaluation...
    FormalAccess &access = *iter;

    // If this access was already finished, continue. This can happen if an
    // owned formal access was forwarded.
    if (access.isFinished()) {
      assert(access.getKind() == FormalAccess::Owned &&
             "Only owned formal accesses should be forwarded.");
      // We can not check that our cleanup is actually dead since the cleanup
      // may have been popped at this point and the stack may have new values.
      continue;
    }

    assert(!access.isFinished() && "Can not finish a formal access cleanup "
                                   "twice");

    // and deactivate the cleanup. This will set the isFinished bit for owned
    // FormalAccess.
    SGF.Cleanups.setCleanupState(access.getCleanup(), CleanupState::Dead);

    // Attempt to diagnose problems where obvious aliasing introduces illegal
    // code. We do a simple N^2 comparison here to detect this because it is
    // extremely unlikely more than a few writebacks are active at once.
    if (access.getKind() == FormalAccess::Exclusive) {
      iterator j = iter;
      ++j;

      for (; j != unwrappedSavedDepth; ++j) {
        FormalAccess &other = *j;
        if (other.getKind() != FormalAccess::Exclusive)
          continue;
        auto &lhs = static_cast<ExclusiveBorrowFormalAccess &>(access);
        auto &rhs = static_cast<ExclusiveBorrowFormalAccess &>(other);
        lhs.diagnoseConflict(rhs, SGF);
      }
    }

    // Claim the address of each and then perform the writeback from the
    // temporary allocation to the source we copied from.
    //
    // This evaluates arbitrary code, so it's best to be paranoid
    // about iterators on the context.
    access.finish(SGF);
  }

  // Then check that we did not add any additional cleanups to the beginning of
  // the stack...
  assert(originalBegin == context.stable_begin() &&
         "more writebacks placed onto context during writeback scope pop?!");

  // And then pop off all stack elements until we reach the savedDepth.
  context.pop(savedDepth.getValue());
}

//===----------------------------------------------------------------------===//
//                         Formal Evaluation Context
//===----------------------------------------------------------------------===//

void FormalEvaluationContext::dump(SILGenFunction &SGF) {
  for (auto II = begin(), IE = end(); II != IE; ++II) {
    FormalAccess &access = *II;
    SGF.Cleanups.dump(access.getCleanup());
  }
}
