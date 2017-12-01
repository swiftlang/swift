//===--- SwiftRT-COFF.cpp -------------------------------------------------===//
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

#include "ImageInspectionCOFF.h"

#include <cstdint>

#define PASTE_EXPANDED(a,b) a##b
#define PASTE(a,b) PASTE_EXPANDED(a,b)

#define STRING_EXPANDED(string) #string
#define STRING(string) STRING_EXPANDED(string)

#define C_LABEL(name) PASTE(__USER_LABEL_PREFIX__,name)

#define PRAGMA(pragma) _Pragma(#pragma)

#define DECLARE_SWIFT_SECTION(name)                                            \
  PRAGMA(section("." #name "$A", long, read, write))                           \
  __declspec(allocate("." #name "$A"))                                         \
  static uintptr_t __start_##name = 0;                                         \
                                                                               \
  PRAGMA(section("." #name "$C", long, read, write))                           \
  __declspec(allocate("." #name "$C"))                                         \
  static uintptr_t __stop_##name = 0;

extern "C" {
DECLARE_SWIFT_SECTION(sw2prtc)
DECLARE_SWIFT_SECTION(sw2tymd)

DECLARE_SWIFT_SECTION(sw3tyrf)
DECLARE_SWIFT_SECTION(sw3rfst)
DECLARE_SWIFT_SECTION(sw3flmd)
DECLARE_SWIFT_SECTION(sw3asty)
}

namespace {
static swift::MetadataSections sections{};
}

static void swift_image_constructor() {
#define SWIFT_SECTION_RANGE(name)                                              \
  { reinterpret_cast<uintptr_t>(&__start_##name) + sizeof(__start_##name),     \
    reinterpret_cast<uintptr_t>(&__stop_##name) - reinterpret_cast<uintptr_t>(&__start_##name) - sizeof(__start_##name) }

  sections = {
      swift::CurrentSectionMetadataVersion,
      0,

      nullptr,
      nullptr,

      SWIFT_SECTION_RANGE(sw2prtc),
      SWIFT_SECTION_RANGE(sw2tymd),

      SWIFT_SECTION_RANGE(sw3tyrf),
      SWIFT_SECTION_RANGE(sw3rfst),
      SWIFT_SECTION_RANGE(sw3flmd),
      SWIFT_SECTION_RANGE(sw3asty),
  };

#undef SWIFT_SECTION_RANGE

  swift_addNewDSOImage(&sections);
}

__declspec(allocate(".CRT$XCIS"))
extern "C" void (*pSwiftImageConstructor)(void) = &swift_image_constructor;
#pragma comment(linker, "/include:" STRING(C_LABEL(pSwiftImageConstructor)))

