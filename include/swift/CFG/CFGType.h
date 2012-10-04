//===--- CFGType.h - Type references on CFG instructions --------*- C++ -*-===//
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
// This file defines the CFGType class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFG_CFGTYPE_H
#define SWIFT_CFG_CFGTYPE_H

#include "llvm/ADT/PointerIntPair.h"
#include "swift/AST/Type.h"

namespace swift {

/// CFGType - The CFG uses a slightly simplified version of the swift type
/// system including all of the standard types like tuples, functions, structs,
/// classes, etc.  However, it diverges by explicitly modeling whether a
/// CFGValue produces a pointer (i.e. is referring to a reference type like a
/// class, or is an LValue of a value type) or whether it produces an value
/// (e.g. the result of a function that returns a struct).
///
/// CFGType models this directly in a way that clients can't ignore.
class CFGType {
  llvm::PointerIntPair<Type, 1, bool> Bits;
  CFGType(Type T, bool isPointer) : Bits(T, isPointer) {}
public:

  // Constructor functions to get an LValue or RValue version of a type.
  static CFGType getValue(Type T) { return CFGType(T, false); }
  static CFGType getPointer(Type T) { return CFGType(T, true); }

  bool isPointer() const { return Bits.getInt(); }
  Type getType() const { return Bits.getPointer(); }
};

} // end namespace swift

#endif
