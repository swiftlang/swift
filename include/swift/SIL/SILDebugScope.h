//===--- SILDebugScope.h - DebugScopes for SIL code -------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_DEBUGSCOPE_H
#define SWIFT_SIL_DEBUGSCOPE_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILLocation.h"

namespace swift {

class SILDebugLocation;
class SILDebugScope;
class SILFunction;
class SILInstruction;

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
  /// An optional chain of inlined call sites.
  ///
  /// If this scope is inlined, this points to a special "scope" that
  /// holds the location of the call site.
  const SILDebugScope *InlinedCallSite;

  SILDebugScope(SILLocation Loc, SILFunction *SILFn,
                const SILDebugScope *ParentScope = nullptr,
                const SILDebugScope *InlinedCallSite = nullptr);

  /// Create a scope for an artificial function.
  SILDebugScope(SILLocation Loc);

  /// Return the function this scope originated from before being inlined.
  SILFunction *getInlinedFunction() const;

  /// Return the parent function of this scope. If the scope was
  /// inlined this recursively returns the function it was inlined
  /// into.
  SILFunction *getParentFunction() const;

#ifndef NDEBUG
  SWIFT_DEBUG_DUMPER(dump(SourceManager &SM,
                          llvm::raw_ostream &OS = llvm::errs(),
                          unsigned Indent = 0));
  SWIFT_DEBUG_DUMPER(dump(SILModule &Mod));
#endif
};

/// Determine whether an instruction may not have a SILDebugScope.
bool maybeScopeless(SILInstruction &I);

/// Knows how to make a deep copy of a debug scope.
class ScopeCloner {
  llvm::SmallDenseMap<const SILDebugScope *,
                      const SILDebugScope *> ClonedScopeCache;
  SILFunction &NewFn;
public:
  /// ScopeCloner expects NewFn to be a clone of the original
  /// function, with all debug scopes and locations still pointing to
  /// the original function.
  ScopeCloner(SILFunction &NewFn);
  /// Return a (cached) deep copy of a scope.
  const SILDebugScope *getOrCreateClonedScope(const SILDebugScope *OrigScope);
};

}

#endif
