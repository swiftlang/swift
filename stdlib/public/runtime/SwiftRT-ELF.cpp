//===--- SwiftRT-ELF.cpp --------------------------------------------------===//
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

#include "ImageInspectionELF.h"

#include <cstddef>

// Create empty sections to ensure that the start/stop symbols are synthesized
// by the linker.  Otherwise, we may end up with undefined symbol references as
// the linker table section was never constructed.

#define DECLARE_SWIFT_SECTION(name)                                            \
  __asm__("\t.section " #name ",\"a\"\n");                                     \
  __attribute__((__visibility__("hidden"))) extern const char __start_##name;  \
  __attribute__((__visibility__("hidden"))) extern const char __stop_##name;

extern "C" {
DECLARE_SWIFT_SECTION(swift4_protocols)
DECLARE_SWIFT_SECTION(swift4_protocol_conformances)
DECLARE_SWIFT_SECTION(swift4_type_metadata)

DECLARE_SWIFT_SECTION(swift4_typeref)
DECLARE_SWIFT_SECTION(swift4_reflstr)
DECLARE_SWIFT_SECTION(swift4_fieldmd)
DECLARE_SWIFT_SECTION(swift4_assocty)
}

#undef DECLARE_SWIFT_SECTION

namespace {
static swift::MetadataSections sections{};
}

__attribute__((__constructor__))
static void swift_image_constructor() {
#define SWIFT_SECTION_RANGE(name)                                              \
  { reinterpret_cast<uintptr_t>(&__start_##name),                              \
    static_cast<uintptr_t>(&__stop_##name - &__start_##name) }

  sections = {
      swift::CurrentSectionMetadataVersion,
      0,

      nullptr,
      nullptr,

      SWIFT_SECTION_RANGE(swift4_protocols),
      SWIFT_SECTION_RANGE(swift4_protocol_conformances),
      SWIFT_SECTION_RANGE(swift4_type_metadata),

      SWIFT_SECTION_RANGE(swift4_typeref),
      SWIFT_SECTION_RANGE(swift4_reflstr),
      SWIFT_SECTION_RANGE(swift4_fieldmd),
      SWIFT_SECTION_RANGE(swift4_assocty),
  };

#undef SWIFT_SECTION_RANGE

  swift_addNewDSOImage(&sections);
}

