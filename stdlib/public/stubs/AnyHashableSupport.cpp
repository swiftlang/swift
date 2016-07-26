//===----------------------------------------------------------------------===//
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

#include "swift/Runtime/Config.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Metadata.h"
#include "../runtime/Private.h"

using namespace swift;

/// The name demangles to "protocol descriptor for Swift.Hashable".
extern "C" const ProtocolDescriptor _TMps8Hashable;

namespace {
struct HashableConformanceKey {
  /// The lookup key, the metadata of a type that is possibly derived
  /// from a type that conforms to `Hashable`.
  const Metadata *derivedType;
};

struct HashableConformanceEntry {
  /// The lookup key, the metadata of a type that is possibly derived
  /// from a type that conforms to `Hashable`.
  const Metadata *derivedType;

  /// The highest (closest to the root) type in the superclass chain
  /// that conforms to `Hashable`.
  const Metadata *baseTypeThatConformsToHashable;

  HashableConformanceEntry(HashableConformanceKey key,
                           const Metadata *baseTypeThatConformsToHashable)
      : derivedType(key.derivedType),
        baseTypeThatConformsToHashable(baseTypeThatConformsToHashable) {}

  int compareWithKey(const HashableConformanceKey &key) const {
    if (key.derivedType != derivedType) {
      return (uintptr_t(key.derivedType) < uintptr_t(derivedType) ? -1 : 1);
    } else {
      return 0;
    }
  }

  static size_t
  getExtraAllocationSize(HashableConformanceKey key,
                         const Metadata *baseTypeThatConformsToHashable) {
    return 0;
  }
};
} // end unnamed namesapce

static Lazy<ConcurrentMap<HashableConformanceEntry>> HashableConformances;

/// Find the base type that introduces the `Hashable` conformance.
///
/// - Precondition: `type` conforms to `Hashable` (not checked).
static const Metadata *findHashableBaseType(const Metadata *type) {
  if (HashableConformanceEntry *entry =
          HashableConformances->find(HashableConformanceKey{type})) {
    return entry->baseTypeThatConformsToHashable;
  }
  const Metadata *baseTypeThatConformsToHashable = type;
  while (true) {
    const Metadata *superclass =
        _swift_class_getSuperclass(baseTypeThatConformsToHashable);
    if (!superclass)
      break;
    if (!swift_conformsToProtocol(superclass, &_TMps8Hashable))
      break;
    baseTypeThatConformsToHashable = superclass;
  }
  HashableConformances->getOrInsert(HashableConformanceKey{type},
                                    baseTypeThatConformsToHashable);
  return baseTypeThatConformsToHashable;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void _swift_stdlib_makeAnyHashableUsingDefaultRepresentation(
  const OpaqueValue *value,
  const void *anyHashableResultPointer,
  const Metadata *T,
  const WitnessTable *hashableWT
);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void _swift_stdlib_makeAnyHashableUpcastingToHashableBaseType(
  const OpaqueValue *value,
  const void *anyHashableResultPointer,
  const Metadata *type,
  const WitnessTable *hashableWT
) {
  switch (type->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    // FIXME(id-as-any): handle ForeignClass.
    _swift_stdlib_makeAnyHashableUsingDefaultRepresentation(
        value, anyHashableResultPointer, findHashableBaseType(type),
        hashableWT);
    return;
  }

  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
    _swift_stdlib_makeAnyHashableUsingDefaultRepresentation(
        value, anyHashableResultPointer, type, hashableWT);
    return;

  case MetadataKind::ErrorObject:
    // FIXME(id-as-any): handle ErrorObject.
    _failCorruptType(type);

  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
    // We assume that the value can not be an existential,
    // because existentials can't conform to Hashable today.
    //
    // FIXME: handle generalized existentials when Swift has them.
    _failCorruptType(type);
  }
  _failCorruptType(type);
}

