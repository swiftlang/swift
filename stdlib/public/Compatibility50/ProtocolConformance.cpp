//===--- ProtocolConformance.cpp - Swift protocol conformance checking ----===//
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
// Checking and caching of Swift protocol conformances.
//
// This implementation is intended to be backward-deployed into Swift 5.0
// runtimes.
//
//===----------------------------------------------------------------------===//

#include "Overrides.h"
#include "../runtime/Private.h"
#include "swift/Basic/Lazy.h"
#include <objc/runtime.h>

using namespace swift;

// Clone of private function getRootSuperclass. This returns the SwiftObject
// class in the ABI-stable dylib, regardless of what the local runtime build
// does, since we're always patching an ABI-stable dylib.
__attribute__((visibility("hidden"), weak))
const ClassMetadata *swift::getRootSuperclass() {
  auto theClass = SWIFT_LAZY_CONSTANT(objc_getClass("_TtCs12_SwiftObject"));
  return (const ClassMetadata *)theClass;
}

// Clone of private helper swift::_swiftoverride_class_getSuperclass
// for use in the override implementation.
static const Metadata *_swift50override_class_getSuperclass(
                                                    const Metadata *theClass) {
  if (const ClassMetadata *classType = theClass->getClassObject()) {
    if (classHasSuperclass(classType))
      return getMetadataForClass(classType->Superclass);
  }
  
  if (const ForeignClassMetadata *foreignClassType
      = dyn_cast<ForeignClassMetadata>(theClass)) {
    if (const Metadata *superclass = foreignClassType->Superclass)
      return superclass;
  }
  
  return nullptr;
}

const WitnessTable *
swift::swift50override_conformsToProtocol(const Metadata *type,
  const ProtocolDescriptor *protocol,
  ConformsToProtocol_t *original_conformsToProtocol)
{
  // The implementation of swift_conformsToProtocol in Swift 5.0 would return
  // a false negative answer when asking whether a subclass conforms using
  // a conformance from a superclass. Work around this by walking up the
  // superclass chain in cases where the original implementation returns
  // null.
  do {
    auto result = original_conformsToProtocol(type, protocol);
    if (result)
      return result;
  } while ((type = _swift50override_class_getSuperclass(type)));
  
  return nullptr;
}
