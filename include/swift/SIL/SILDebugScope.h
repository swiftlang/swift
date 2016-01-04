//===--- SILDebugScope.h - DebugScopes for SIL code -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines a container for scope information used to
// generate debug info.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DEBUGSCOPE_H
#define SWIFT_SIL_DEBUGSCOPE_H

#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILFunction.h"

namespace swift {

class SILDebugLocation;
class SILDebugScope;

/// SILDebugScope - This class stores a lexical scope as it is
/// represented in the debug info.
class SILDebugScope : public SILAllocated<SILDebugScope> {
public:
  SILLocation Loc;
  /// Always points to the parent lexical scope.
  const SILDebugScope *Parent;
  /// If this scope is inlined, this points to a special "scope" that
  /// holds only the location of the call site. The parent scope will be
  /// the scope of the inlined call site.
  const SILDebugScope *InlinedCallSite;
  /// The SILFunction that the scope belongs to. Inlined functions may
  /// be elided, so keep track of their type here.
  /// FIXME: Storing this for every scope is wasteful.  We only need
  /// this once per function.
  SILFunction *SILFn;

  SILDebugScope(SILLocation Loc, SILFunction &SILFn,
                const SILDebugScope *Parent = nullptr,
                const SILDebugScope *InlinedCallSite = nullptr)
      : Loc(Loc), Parent(Parent), InlinedCallSite(InlinedCallSite),
        SILFn(&SILFn) {}

  /// Create a scope for an artificial function.
  SILDebugScope(SILLocation Loc)
      : Loc(Loc), Parent(nullptr), InlinedCallSite(nullptr), SILFn(nullptr) {}

  /// Create an inlined version of CalleeScope.
  SILDebugScope(const SILDebugScope *CallSiteScope,
                const SILDebugScope *CalleeScope, SILFunction *InlinedFn)
      : Loc(CalleeScope->Loc), Parent(CalleeScope->Parent),
        InlinedCallSite(CallSiteScope), SILFn(InlinedFn) {
    assert(CallSiteScope && CalleeScope);
    assert(InlinedFn->isInlined() &&
           "function of inlined debug scope is not inlined");
  }
};

#ifndef NDEBUG
/// Determine whether an instruction may not have a SILDebugScope.
bool maybeScopeless(SILInstruction &I);
#endif

/// Knows how to make a deep copy of a debug scope.
class ScopeCloner {
  llvm::SmallDenseMap<const SILDebugScope *,
                      const SILDebugScope *> ClonedScopeCache;
  SILFunction &NewFn;
public:
  /// ScopeCloner expects NewFn to be a clone of the original
  /// function, with all debug scopes and locations still pointing to
  /// the original function.
  ScopeCloner(SILFunction &NewFn) : NewFn(NewFn) {
    // Some clients of SILCloner copy over the original function's
    // debug scope. Create a new one here.
    // FIXME: Audit all call sites and make them create the function
    // debug scope.
    if (NewFn.getDebugScope()->SILFn != &NewFn) {
      NewFn.getDebugScope()->SILFn->setInlined();
      NewFn.setDebugScope(getOrCreateClonedScope(NewFn.getDebugScope()));
    }
  }

  /// Return a (cached) deep copy of a scope.
  const SILDebugScope *getOrCreateClonedScope(const SILDebugScope *OrigScope);
};

/// A SILLocation paired with a SILDebugScope.
class SILDebugLocation : public SILAllocated<SILDebugLocation> {
  SILLocation Location;
  const SILDebugScope *Scope = nullptr;

public:
  SILDebugLocation(SILLocation Loc, const SILDebugScope *DS)
      : Location(Loc), Scope(DS) {}
  SILLocation getLocation() const { return Location; }
  const SILDebugScope *getScope() const { return Scope; }
  bool operator==(const SILDebugLocation &other) const {
    return Location == other.getLocation() && Scope == other.getScope();
  }
};

/// Fingerprint a SILDebugLocation for use in a DenseMap.
typedef std::pair<std::pair<const void *, unsigned>, const void *> DebugLocKey;
struct SILDebugLocationID : public DebugLocKey {
  SILDebugLocationID(const SILDebugLocation &L)
      : DebugLocKey({L.getLocation().getOpaquePointerValue(),
                     L.getLocation().getOpaqueKind()},
                    L.getScope()) {}
};
}

#endif
