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

#include "../../public/runtime/Private.h"
#include "Overrides.h"
#include "swift/Threading/Once.h"
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <objc/runtime.h>

using namespace swift;

#if __POINTER_WIDTH__ == 64
using mach_header_platform = mach_header_64;
#else
using mach_header_platform = mach_header;
#endif

/// The Mach-O section name for the section containing protocol conformances.
/// This lives within SEG_TEXT.
constexpr const char ProtocolConformancesSection[] = "__swift5_proto";

// A dummy target context descriptor to use in conformance records which point
// to a NULL descriptor. It doesn't have to be completely valid, just something
// that code reading conformance descriptors will ignore.
struct {
  ContextDescriptorFlags flags;
  int32_t offset;
} DummyTargetContextDescriptor = {
  ContextDescriptorFlags().withKind(ContextDescriptorKind::Extension),
  0
};

// Search for any protocol conformance descriptors with a NULL type descriptor
// and rewrite those to point to the dummy descriptor. This occurs when an
// extension is used to declare a conformance on a weakly linked type and that
// type is not present at runtime.
static void addImageCallback(const mach_header *mh, intptr_t vmaddr_slide) {
  unsigned long size;
  const uint8_t *section =
    getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                   SEG_TEXT, ProtocolConformancesSection,
                   &size);
  if (!section)
    return;

  auto recordsBegin
    = reinterpret_cast<const ProtocolConformanceRecord*>(section);
  auto recordsEnd
    = reinterpret_cast<const ProtocolConformanceRecord*>
                                          (section + size);
  for (auto record = recordsBegin; record != recordsEnd; ++record) {
    auto descriptor = record->get();
    if (auto typePtr = descriptor->_getTypeDescriptorLocation()) {
      if (*typePtr == nullptr)
        *typePtr = reinterpret_cast<TargetContextDescriptor<InProcess> *>(
          &DummyTargetContextDescriptor);
    }
  }
}

// Register the add image callback with dyld.
static void registerAddImageCallback(void *) {
  _dyld_register_func_for_add_image(addImageCallback);
}

// Defined in libswiftCompatibility51, which is always linked if we link against
// libswiftCompatibility50
const Metadata *_swiftoverride_class_getSuperclass(const Metadata *theClass);

const WitnessTable *
swift::swift50override_conformsToProtocol(const Metadata *type,
  const ProtocolDescriptor *protocol,
  ConformsToProtocol_t *original_conformsToProtocol)
{
  // Register our add image callback if necessary.
  static swift::once_t token;
  swift::once(token, registerAddImageCallback, nullptr);

  // The implementation of swift_conformsToProtocol in Swift 5.0 would return
  // a false negative answer when asking whether a subclass conforms using
  // a conformance from a superclass. Work around this by walking up the
  // superclass chain in cases where the original implementation returns
  // null.
  do {
    auto result = original_conformsToProtocol(type, protocol);
    if (result)
      return result;
  } while ((type = _swiftoverride_class_getSuperclass(type)));

  return nullptr;
}
