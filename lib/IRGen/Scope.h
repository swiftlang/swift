//===--- Scope.h - Declarations for scope RAII objects ----------*- C++ -*-===//
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
// This file defines the Scope and FullExpr RAII objects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SCOPE_H
#define SWIFT_IRGEN_SCOPE_H

#include "IRGenFunction.h"

namespace swift {
namespace irgen {

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
class Scope {
  IRGenFunction &IGF;
  CleanupsDepth Depth;
  CleanupsDepth SavedInnermostScope;
  IRGenFunction::LocalTypeDataDepth SavedLocalTypeDataDepth;

  void popImpl() {
    IGF.Cleanups.checkIterator(Depth);
    IGF.Cleanups.checkIterator(IGF.InnermostScope);
    assert(IGF.InnermostScope == Depth && "popping scopes out of order");

    IGF.InnermostScope = SavedInnermostScope;
    IGF.Cleanups.checkIterator(IGF.InnermostScope);

    assert(IGF.ScopedLocalTypeData.size() >= SavedLocalTypeDataDepth);
    IGF.endLocalTypeDataScope(SavedLocalTypeDataDepth);
  }

public:
  explicit Scope(IRGenFunction &IGF)
    : IGF(IGF), Depth(IGF.getCleanupsDepth()),
      SavedInnermostScope(IGF.InnermostScope),
      SavedLocalTypeDataDepth(IGF.ScopedLocalTypeData.size()) {
    assert(Depth.isValid());
    IGF.Cleanups.checkIterator(IGF.InnermostScope);
    IGF.InnermostScope = Depth;
  }

  void pop() {
    assert(Depth.isValid() && "popping a scope twice!");
    popImpl();
    Depth = CleanupsDepth::invalid();
  }

  ~Scope() {
    if (Depth.isValid()) popImpl();
  }
};

} // end namespace irgen
} // end namespace swift

#endif
