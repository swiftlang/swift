//===--- KnownMetadata.cpp - Swift Language ABI Known Metadata Objects ----===//
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
}

// We use explicit sizes and alignments here just in case the C ABI
// under-aligns any or all of them.
const ValueWitnessTable swift::_TWVBi8_ =
  ValueWitnessTableForBox<NativeBox<uint8_t, 1>>::table;
const ValueWitnessTable swift::_TWVBi16_ =
  ValueWitnessTableForBox<NativeBox<uint16_t, 2>>::table;
const ValueWitnessTable swift::_TWVBi32_ =
  ValueWitnessTableForBox<NativeBox<uint32_t, 4>>::table;
const ValueWitnessTable swift::_TWVBi64_ =
  ValueWitnessTableForBox<NativeBox<uint64_t, 8>>::table;
const ValueWitnessTable swift::_TWVBi128_ =
  ValueWitnessTableForBox<NativeBox<int128_like, 16>>::table;

/// The basic value-witness table for Swift object pointers.
const ExtraInhabitantsValueWitnessTable swift::_TWVBo =
  ValueWitnessTableForBox<SwiftRetainableBox>::table;

/// The value-witness table for pointer-aligned unmanaged pointer types.
const ExtraInhabitantsValueWitnessTable swift::_TWVMBo =
  ValueWitnessTableForBox<PointerPointerBox>::table;

/// The value-witness table for BridgeObject.
const ExtraInhabitantsValueWitnessTable swift::_TWVBb =
  ValueWitnessTableForBox<BridgeObjectBox>::table;

/// The value-witness table for UnsafeValueBuffer.  You can do layout
/// with this, but the type isn't copyable, so most of the value
/// operations are meaningless.
static const ValueWitnessTable _TWVBB =
  ValueWitnessTableForBox<NativeBox<ValueBuffer>>::table;

#if SWIFT_OBJC_INTEROP
/*** Objective-C pointers ****************************************************/

// This section can reasonably be suppressed in builds that don't
// need to support Objective-C.

/// The basic value-witness table for ObjC object pointers.
const ExtraInhabitantsValueWitnessTable swift::_TWVBO =
  ValueWitnessTableForBox<ObjCRetainableBox>::table;
#endif

/*** Functions ***************************************************************/

namespace {
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
}

/// The basic value-witness table for function types.
const ExtraInhabitantsValueWitnessTable swift::_TWVFT_T_ =
  ValueWitnessTableForBox<ThickFunctionBox>::table;

/// The basic value-witness table for thin function types.
const ExtraInhabitantsValueWitnessTable swift::_TWVXfT_T_ =
  ValueWitnessTableForBox<FunctionPointerBox>::table;

/*** Empty tuples ************************************************************/

/// The basic value-witness table for empty types.
const ValueWitnessTable swift::_TWVT_ =
  ValueWitnessTableForBox<AggregateBox<>>::table;

/*** Known metadata **********************************************************/

// Define some builtin opaque metadata.
#define OPAQUE_METADATA(TYPE) \
  const FullOpaqueMetadata swift::_TMd##TYPE = { \
    { &_TWV##TYPE },                             \
    { { MetadataKind::Opaque } }                 \
  };
OPAQUE_METADATA(Bi8_)
OPAQUE_METADATA(Bi16_)
OPAQUE_METADATA(Bi32_)
OPAQUE_METADATA(Bi64_)
OPAQUE_METADATA(Bi128_)
OPAQUE_METADATA(Bo)
OPAQUE_METADATA(Bb)
OPAQUE_METADATA(BB)
#if SWIFT_OBJC_INTEROP
OPAQUE_METADATA(BO)
#endif

/// The standard metadata for the empty tuple.
const FullMetadata<TupleTypeMetadata> swift::_TMdT_ = {
  { &_TWVT_ },                 // ValueWitnesses
  {
    { MetadataKind::Tuple },   // Kind
    0,                         // NumElements
    nullptr                    // Labels
  }
};
