//===--- FoundationSupport.cpp - Support functions for Foundation ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Helper functions for the Foundation framework.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/FoundationSupport.h"

#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/HeapObject.h"

using namespace swift;

/// Returns a boolean indicating whether the Objective-C name of a class type is
/// stable across executions, i.e., if the class name is safe to serialize. (The
/// names of private and local types are unstable.)
bool
swift::_swift_isObjCTypeNameSerializable(Class theClass) {
  auto type = (AnyClassMetadata *)theClass;
  switch (type->getKind()) {
  case MetadataKind::ObjCClassWrapper:
  case MetadataKind::ForeignClass:
    return true;
  case MetadataKind::Class: {
    // Pure ObjC classes always have stable names.
    if (type->isPureObjC())
      return true;
    auto cls = static_cast<const ClassMetadata *>(type);
    // Peek through artificial subclasses.
    if (cls->isArtificialSubclass()) {
      cls = cls->Superclass;
    }
    // A custom ObjC name is always considered stable.
    if (cls->getFlags() & ClassFlags::HasCustomObjCName)
      return true;
    // Otherwise the name is stable if the class has no anonymous ancestor context.
    auto desc = static_cast<const ContextDescriptor *>(cls->getDescription());
    while (desc) {
      if (desc->getKind() == ContextDescriptorKind::Anonymous) {
        return false;
      }
      desc = desc->Parent.get();
    }
    return true;
  }
  default:
    return false;
  }
}
#endif // SWIFT_OBJC_INTEROP
