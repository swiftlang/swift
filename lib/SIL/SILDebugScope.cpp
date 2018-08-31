//===--- SILDebugScope.cpp - DebugScopes for SIL code ---------------------===//
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
///
/// \file
///
/// This file defines a container for scope information used to
/// generate debug info.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

static_assert(sizeof(SILDebugScope) == 4*sizeof(void*),
              "SILDebugScope should be small");

SILDebugScope::SILDebugScope(SILLocation Loc, SILFunction *SILFn,
                             const SILDebugScope *ParentScope,
                             const SILDebugScope *InlinedCallSite)
    : Loc(Loc) {
  if (ParentScope)
    ParentAndHasInlinedCallSite.setPointer(ParentScope);
  else {
    assert(SILFn && "no parent provided");
    ParentAndHasInlinedCallSite.setPointer(SILFn);
  }
  if (InlinedCallSite) {
    *this->getTrailingObjects<const SILDebugScope *>() = InlinedCallSite;
    ParentAndHasInlinedCallSite.setInt(true);
  }
}

SILDebugScope::SILDebugScope(SILLocation Loc) : Loc(Loc) {}

SILFunction *SILDebugScope::getInlinedFunction() const {
  if (getParent().isNull())
    return nullptr;

  const SILDebugScope *Scope = this;
  while (Scope->getParent().is<const SILDebugScope *>())
    Scope = Scope->getParent().get<const SILDebugScope *>();
  assert(Scope->getParent().is<SILFunction *>() && "orphaned scope");
  return Scope->getParent().get<SILFunction *>();
}

SILFunction *SILDebugScope::getParentFunction() const {
  if (const SILDebugScope *InlinedCallSite = getInlinedCallSite())
    return InlinedCallSite->getParentFunction();
  if (auto *ParentScope = getParent().dyn_cast<const SILDebugScope *>())
    return ParentScope->getParentFunction();
  return getParent().get<SILFunction *>();
}
