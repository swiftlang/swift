//===--- MetadataValues.h - Compiler/runtime ABI Metadata -------*- C++ -*-===//
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
// This header is shared between the runtime and the compiler and
// includes target-independent information which can be usefully shared
// between them.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_METADATAVALUES_H
#define SWIFT_ABI_METADATAVALUES_H

#include <stdint.h>

namespace swift {

/// Kinds of Swift metadata records.  Some of these are types, some
/// aren't.
enum class MetadataKind : uintptr_t {
  /// A class type.
  Class         = 0,

  /// A struct type.
  Struct        = 1,

  /// A oneof type.
  /// If we add reference oneofs, that needs to go here.
  Oneof         = 2,

  /// A type whose value is not exposed in the metadata system.
  Opaque        = 8,

  /// A tuple.
  Tuple         = 9,

  /// A monomorphic function.
  Function      = 10,

  /// A polymorphic function.
  PolyFunction  = 11,

  /// An existential type.
  Existential   = 12,

  /// A metatype.
  Metatype      = 13,

  /// An ObjC class wrapper.
  ObjCClassWrapper = 14,

  // Array types?
  // L-value types?

  // After this point start the non-type metadata.

  /// A heap-allocated local variable.
  HeapLocalVariable = 64,

  /// A heap-allocated array.
  HeapArray = 65,
  
  /// Anything greater than this is a class isa pointer.
  MetadataKind_Last = HeapArray
};

}

#endif
