//===--- KnownMetadata.cpp - Swift Language ABI Known Metadata Objects ----===//
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
//
// Definitions of some builtin metadata objects.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/HeapObject.h"
#include "MetadataImpl.h"
#include "Private.h"
#include <cstring>
#include <climits>

using namespace swift;
using namespace metadataimpl;

/// Copy a value from one object to another based on the size in the
/// given type metadata.
OpaqueValue *swift::swift_copyPOD(OpaqueValue *dest, OpaqueValue *src,
                                  const Metadata *type) {
  return (OpaqueValue*) memcpy(dest, src, type->getValueWitnesses()->size);
}

namespace {
  // A type sized and aligned the way Swift wants Int128 (and Float80/Float128)
  // to be sized and aligned.
  struct alignas(16) int128_like {
    char data[16];
  };

  struct alignas(32) int256_like {
    char data[32];
  };
  struct alignas(64) int512_like {
    char data[64];
  };
} // end anonymous namespace

// We use explicit sizes and alignments here just in case the C ABI
// under-aligns any or all of them.
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi8_) =
  ValueWitnessTableForBox<NativeBox<uint8_t, 1>>::table;
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi16_) =
  ValueWitnessTableForBox<NativeBox<uint16_t, 2>>::table;
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi32_) =
  ValueWitnessTableForBox<NativeBox<uint32_t, 4>>::table;
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi64_) =
  ValueWitnessTableForBox<NativeBox<uint64_t, 8>>::table;
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi128_) =
  ValueWitnessTableForBox<NativeBox<int128_like, 16>>::table;
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi256_) =
  ValueWitnessTableForBox<NativeBox<int256_like, 32>>::table;
const ValueWitnessTable swift::VALUE_WITNESS_SYM(Bi512_) =
  ValueWitnessTableForBox<NativeBox<int512_like, 64>>::table;

/// The basic value-witness table for Swift object pointers.
const ExtraInhabitantsValueWitnessTable swift::VALUE_WITNESS_SYM(Bo) =
  ValueWitnessTableForBox<SwiftRetainableBox>::table;

/// The value-witness table for pointer-aligned unmanaged pointer types.
const ExtraInhabitantsValueWitnessTable swift::METATYPE_VALUE_WITNESS_SYM(Bo) =
  ValueWitnessTableForBox<PointerPointerBox>::table;

/// The value-witness table for raw pointers.
const ExtraInhabitantsValueWitnessTable swift::VALUE_WITNESS_SYM(Bp) =
  ValueWitnessTableForBox<RawPointerBox>::table;

/// The value-witness table for BridgeObject.
const ExtraInhabitantsValueWitnessTable swift::VALUE_WITNESS_SYM(Bb) =
  ValueWitnessTableForBox<BridgeObjectBox>::table;

/// The value-witness table for UnsafeValueBuffer.  You can do layout
/// with this, but the type isn't copyable, so most of the value
/// operations are meaningless.
static const ValueWitnessTable VALUE_WITNESS_SYM(BB) =
  ValueWitnessTableForBox<NativeBox<ValueBuffer>>::table;

#if SWIFT_OBJC_INTEROP
/*** Objective-C pointers ****************************************************/

// This section can reasonably be suppressed in builds that don't
// need to support Objective-C.

/// The basic value-witness table for ObjC object pointers.
const ExtraInhabitantsValueWitnessTable swift::VALUE_WITNESS_SYM(BO) =
  ValueWitnessTableForBox<ObjCRetainableBox>::table;

#endif

/*** Functions ***************************************************************/

namespace {
  // @escaping function types.
  struct ThickFunctionBox
    : AggregateBox<FunctionPointerBox, SwiftRetainableBox> {

    static constexpr unsigned numExtraInhabitants =
      FunctionPointerBox::numExtraInhabitants;

    static void storeExtraInhabitant(char *dest, int index) {
      FunctionPointerBox::storeExtraInhabitant((void**) dest, index);
    }

    static int getExtraInhabitantIndex(const char *src) {
      return FunctionPointerBox::getExtraInhabitantIndex((void * const *) src);
    }
  };
  /// @noescape function types.
  struct TrivialThickFunctionBox
      : AggregateBox<FunctionPointerBox, RawPointerBox> {

    static constexpr unsigned numExtraInhabitants =
        FunctionPointerBox::numExtraInhabitants;

    static void storeExtraInhabitant(char *dest, int index) {
      FunctionPointerBox::storeExtraInhabitant((void **)dest, index);
    }

    static int getExtraInhabitantIndex(const char *src) {
      return FunctionPointerBox::getExtraInhabitantIndex((void *const *)src);
    }
  };
} // end anonymous namespace

/// The basic value-witness table for escaping function types.
const ExtraInhabitantsValueWitnessTable
  swift::VALUE_WITNESS_SYM(FUNCTION_MANGLING) =
    ValueWitnessTableForBox<ThickFunctionBox>::table;

/// The basic value-witness table for @noescape function types.
const ExtraInhabitantsValueWitnessTable
  swift::VALUE_WITNESS_SYM(NOESCAPE_FUNCTION_MANGLING) =
    ValueWitnessTableForBox<TrivialThickFunctionBox>::table;

/// The basic value-witness table for thin function types.
const ExtraInhabitantsValueWitnessTable
  swift::VALUE_WITNESS_SYM(THIN_FUNCTION_MANGLING) =
    ValueWitnessTableForBox<FunctionPointerBox>::table;

/*** Empty tuples ************************************************************/

/// The basic value-witness table for empty types.
const ValueWitnessTable swift::VALUE_WITNESS_SYM(EMPTY_TUPLE_MANGLING) =
  ValueWitnessTableForBox<AggregateBox<>>::table;

/*** Known metadata **********************************************************/

// Define some builtin opaque metadata.
#define OPAQUE_METADATA(TYPE) \
  const FullOpaqueMetadata swift::METADATA_SYM(TYPE) = { \
    { &VALUE_WITNESS_SYM(TYPE) },                             \
    { { MetadataKind::Opaque } }                 \
  };
#define BUILTIN_TYPE(Symbol, Name) \
  OPAQUE_METADATA(Symbol)
#include "swift/Runtime/BuiltinTypes.def"

/// The standard metadata for the empty tuple.
const FullMetadata<TupleTypeMetadata> swift::
METADATA_SYM(EMPTY_TUPLE_MANGLING) = {
  { &VALUE_WITNESS_SYM(EMPTY_TUPLE_MANGLING) },                 // ValueWitnesses
  {
    { MetadataKind::Tuple },   // Kind
    0,                         // NumElements
    nullptr                    // Labels
  }
};

