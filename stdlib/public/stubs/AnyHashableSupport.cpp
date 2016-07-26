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
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Metadata.h"
#include "../runtime/Private.h"

using namespace swift;

extern "C" const ProtocolDescriptor _TMps8Hashable;

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
  // FIXME(id-as-any)(performance): cache the result of the lookup.
  switch (type->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass: {
    // FIXME(id-as-any): handle ForeignClass.
    while (true) {
      const Metadata *superclass = _swift_class_getSuperclass(type);
      if (!superclass)
        break;
      if (!swift_conformsToProtocol(superclass, &_TMps8Hashable))
        break;
      type = superclass;
    }
    _swift_stdlib_makeAnyHashableUsingDefaultRepresentation(
        value, anyHashableResultPointer, type, hashableWT);
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

