//===--- Scope.cpp --------------------------------------------------------===//
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

#include "Scope.h"

using namespace swift;
using namespace Lowering;

ManagedValue Scope::popPreservingValue(ManagedValue mv) {
  // If we have a value, make sure that it is an object. The reason why is
  // that we want to make sure that we are not forwarding a cleanup for a
  // stack location that will be destroyed by this scope.
  assert(mv && mv.getType().isObject() &&
         (mv.getType().isTrivial(cleanups.SGF.getModule()) ||
          mv.getOwnershipKind() == ValueOwnershipKind::Trivial ||
          mv.hasCleanup()));
  CleanupCloner cloner(cleanups.SGF, mv);
  SILValue value = mv.forward(cleanups.SGF);
  pop();
  return cloner.clone(value);
}

RValue Scope::popPreservingValue(RValue &&rv) {
  assert(rv.isPlusOne(cleanups.SGF) &&
         "Can only push plus one rvalues through a scope");
  assert(rv.getTypeLowering(cleanups.SGF).isLoadable() &&
         "Can only push loadable +1 rvalues through a scope");

  // First gather all of the data that we need to recreate the RValue in the
  // outer scope.
  CanType type = rv.type;
  unsigned numEltsRemaining = rv.elementsToBeAdded;
  CleanupCloner cloner(cleanups.SGF, rv);
  llvm::SmallVector<SILValue, 4> values;
  std::move(rv).forwardAll(cleanups.SGF, values);

  // Then pop the cleanups.
  pop();

  // Reconstruct the managed values from the underlying sil values in the outer
  // scope. Since the RValue wants a std::vector value, we use that instead.
  std::vector<ManagedValue> managedValues;
  std::transform(
      values.begin(), values.end(), std::back_inserter(managedValues),
      [&cloner](SILValue v) -> ManagedValue { return cloner.clone(v); });

  // And then assemble the managed values into a rvalue.
  return RValue(cleanups.SGF, std::move(managedValues), type, numEltsRemaining);
}

void Scope::popImpl() {
  cleanups.stack.checkIterator(depth);
  cleanups.stack.checkIterator(cleanups.innermostScope);
  assert(cleanups.innermostScope == depth && "popping scopes out of order");

  cleanups.innermostScope = savedInnermostScope;
  cleanups.endScope(depth, loc);
  cleanups.stack.checkIterator(cleanups.innermostScope);
  cleanups.popTopDeadCleanups(cleanups.innermostScope);
}
