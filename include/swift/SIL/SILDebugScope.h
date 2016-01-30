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

/// This class stores a lexical scope as it is represented in the
/// debug info. In contrast to LLVM IR, SILDebugScope also holds all
/// the inlining information. In LLVM IR the inline info is part of
/// DILocation.
class SILDebugScope : public SILAllocated<SILDebugScope> {
public:
  /// The AST node this lexical scope represents.
  SILLocation Loc;
  /// Always points to the parent lexical scope.
  /// For top-level scopes, this is the SILFunction.
  PointerUnion<const SILDebugScope *, SILFunction *> Parent;
  /// If this scope is inlined, this points to a special "scope" that
  /// holds only the location of the call site. The parent scope will be
  /// the scope of the inlined call site.
  const SILDebugScope *InlinedCallSite;

  SILDebugScope(SILLocation Loc, SILFunction &SILFn,
                const SILDebugScope *ParentScope = nullptr,
                const SILDebugScope *InlinedCallSite = nullptr)
      : Loc(Loc), InlinedCallSite(InlinedCallSite) {
    if (ParentScope)
      Parent = ParentScope;
    else
      Parent = &SILFn;
  }

  /// Create a scope for an artificial function.
  SILDebugScope(SILLocation Loc)
      : Loc(Loc), InlinedCallSite(nullptr) {}

  /// Create an inlined version of CalleeScope.
  SILDebugScope(const SILDebugScope *CallSiteScope,
                const SILDebugScope *CalleeScope)
      : Loc(CalleeScope->Loc), Parent(CalleeScope->Parent),
        InlinedCallSite(CallSiteScope) {
    assert(CallSiteScope && CalleeScope);
    assert(CalleeScope->getFunction()->isInlined() &&
           "function of inlined debug scope is not inlined");
  }

  SILFunction *getFunction() const {
    if (Parent.isNull())
      return nullptr;

    const SILDebugScope *Scope = this;
    while (Scope->Parent.is<const SILDebugScope *>())
      Scope = Scope->Parent.get<const SILDebugScope *>();
    assert(Scope->Parent.is<SILFunction *>() && "orphaned scope");
    return Scope->Parent.get<SILFunction *>();
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
    auto *SILFn = NewFn.getDebugScope()->Parent.get<SILFunction *>();
    if (SILFn != &NewFn) {
      SILFn->setInlined();
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
