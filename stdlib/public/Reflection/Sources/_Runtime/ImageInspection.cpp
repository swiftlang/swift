//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

#include <stdint.h>

#include "swift/Runtime/Config.h"
#include "swift/Threading/Once.h"

extern "C"
void registerAttributes(const void *section, intptr_t size);

//===----------------------------------------------------------------------===//
// Mach-O Image Inspection
//===----------------------------------------------------------------------===//

#if defined(__MACH__)

#include <mach-o/dyld.h>
#include <mach-o/getsect.h>

#if __POINTER_WIDTH__ == 64
typedef mach_header_64 mach_header_platform;
#else
typedef mach_header mach_header_platform;
#endif

void lookupSection(const struct mach_header *header, const char *segment,
                   const char *section,
                   void (*registerFunc)(const void *, intptr_t)) {
  unsigned long size = 0;

  auto sectionData = getsectiondata(
    reinterpret_cast<const mach_header_platform *>(header), segment, section,
    &size);

  registerFunc(sectionData, size);
}

void imageFunc(const struct mach_header *header, intptr_t size) {
  lookupSection(header, "__TEXT", "__swift5_rattrs", registerAttributes);
}

extern "C" SWIFT_CC(swift)
void initializeDyldLookup() {
  static swift::once_t token;
  swift::once(token, []{
    _dyld_register_func_for_add_image(imageFunc);
  });
}

#endif
