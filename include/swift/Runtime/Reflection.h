//===--- Reflection.h - Swift Language Reflection Support -------*- C++ -*-===//
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
// An implementation of value reflection based on type metadata.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include <cstdlib>

namespace swift {
  
struct MirrorWitnessTable;
  
/// The layout of protocol<Mirror>.
struct Mirror {
  OpaqueExistentialContainer Header;
  const MirrorWitnessTable *MirrorWitness;
};
  
/// func reflect<T>(x: T) -> Mirror
///
/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
extern "C" Mirror
swift_reflectAny(OpaqueValue *value, const Metadata *T);
  
/// func unsafeReflect<T>(owner: Builtin.NativeObject,
///                       x: UnsafeMutablePointer<T>) -> Mirror
///
/// Produce a mirror for any value. If the value's type conforms to Reflectable,
/// invoke its getMirror() method; otherwise, fall back to an implementation
/// in the runtime that structurally reflects values of any type.
extern "C" Mirror
swift_unsafeReflectAny(HeapObject *owner,
                       const OpaqueValue *value, const Metadata *T);
  
}
