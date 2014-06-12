//===--- SILDebugScope.h - DebugScopes for SIL code -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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

namespace swift {

class SILFunction;

/// SILDebugScope - This class stores a lexical scope as it is
/// represented in the debug info.
class SILDebugScope : public SILAllocated<SILDebugScope> {
public:
  SILLocation Loc;
  /// Always points to the parent lexical scope.
  SILDebugScope *Parent;
  /// If this scope is inlined, this points to a special "scope" that
  /// holds only the location of the call site. The parent scope will be
  /// the scope of the inlined call site.
  SILDebugScope *InlineScope;
  /// The SILFunction that the scope belongs to. Inlined functions may
  /// be elided, so keep track of their type here.
  /// FIXME: Storing this for every scope is wasteful.  We only need
  /// this once per function.
  SILFunction *SILFn;

  SILDebugScope(SILLocation Loc,
                SILFunction &SILFn,
                SILDebugScope *Parent = nullptr,
                SILDebugScope *InlineScope = nullptr)
    : Loc(Loc), Parent(Parent), InlineScope(InlineScope),
      SILFn(&SILFn)
    { }

  /// Create a scope for an artificial function.
  SILDebugScope(SILLocation Loc)
    : Loc(Loc), Parent(nullptr), InlineScope(nullptr), SILFn(nullptr)
    { }

  /// Create an inlined version of CalleeScope.
  SILDebugScope(SILDebugScope *CallSiteScope, SILDebugScope *CalleeScope,
                SILFunction &SILFn)
    : Loc(CalleeScope->Loc), Parent(CalleeScope->Parent),
      InlineScope(CallSiteScope), SILFn(&SILFn) {
    assert(CallSiteScope && CalleeScope);
  }

  void setParent(SILDebugScope *P) { Parent = P; }
};

}

#endif
