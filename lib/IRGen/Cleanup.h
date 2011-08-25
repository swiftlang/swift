//===--- Cleanup.h - Declarations for Cleanup IR Generation -----*- C++ -*-===//
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
// This file defines some types that are generically useful for
// dealing with cleanups in IR Generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLEANUP_H
#define SWIFT_IRGEN_CLEANUP_H

namespace swift {
namespace irgen {

class IRGenFunction;

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
class Scope {
public:
  Scope(IRGenFunction &IGF) {}
};

/// A FullExpr is a RAII object recording that a full-expression has
/// been entered.
class FullExpr {
public:
  FullExpr(IRGenFunction &IGF) {}
};

} // end namespace irgen
} // end namespace swift

#endif
