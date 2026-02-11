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

#include "ImageInspectionCommon.h"
#include "swift/shims/MetadataSections.h"
#include "swift/Runtime/Backtrace.h"
#include "swift/Runtime/Config.h"

#include <cstddef>
#include <new>

#define SWIFT_METADATA_SEGMENT_TYPE 0x73356d64 // s5md or dms5 on little-endian

extern "C" const char __ehdr_start[] __attribute__((__weak__));

#if SWIFT_ENABLE_BACKTRACING
// Drag in a symbol from the backtracer, to force the static linker to include
// the code.
static const void *__backtraceRef __attribute__((used, retain))
  = (const void *)swift::runtime::backtrace::_swift_backtrace_isThunkFunction;
#endif

// Create empty sections to ensure that the start/stop symbols are synthesized
// by the linker.  Otherwise, we may end up with undefined symbol references as
// the linker table section was never constructed.
# define DECLARE_EMPTY_METADATA_SECTION(name) __asm__("\t.section " #name ",\"aR\"\n");

#define BOUNDS_VISIBILITY __attribute__((__visibility__("hidden"), \
                                         __aligned__(1)))

#define DECLARE_BOUNDS(name)                            \
  BOUNDS_VISIBILITY extern const char __start_##name;   \
  BOUNDS_VISIBILITY extern const char __stop_##name;

#define DECLARE_SWIFT_SECTION(name)             \
  DECLARE_EMPTY_METADATA_SECTION(name)          \
  DECLARE_BOUNDS(name)

// These may or may not be present, depending on compiler switches; it's
// worth calling them out as a result.
#define DECLARE_SWIFT_REFLECTION_SECTION(name)  \
  DECLARE_SWIFT_SECTION(name)

extern "C" {
DECLARE_SWIFT_SECTION(swift5_protocols)
DECLARE_SWIFT_SECTION(swift5_protocol_conformances)
DECLARE_SWIFT_SECTION(swift5_type_metadata)

DECLARE_SWIFT_REFLECTION_SECTION(swift5_fieldmd)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_builtin)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_assocty)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_capture)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_reflstr)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_typeref)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_mpenum)

DECLARE_SWIFT_SECTION(swift5_replace)
DECLARE_SWIFT_SECTION(swift5_replac2)
DECLARE_SWIFT_SECTION(swift5_accessible_functions)
DECLARE_SWIFT_SECTION(swift5_runtime_attributes)

DECLARE_SWIFT_SECTION(swift5_tests)
}

#undef DECLARE_SWIFT_SECTION

namespace {
struct ElfNoteHeader {
  uint32_t namesz;
  uint32_t descsz;
  uint32_t type;
};

struct  SectionRange {
  uintptr_t start;
  uintptr_t stop;
};

struct MetadataSections {
  uintptr_t version;
  uintptr_t base;
  uintptr_t unused0;
  uintptr_t unused1;

#define HANDLE_SWIFT_SECTION(name, coff) \
  SectionRange name;
#include "MetadataSectionNames.def"
#undef HANDLE_SWIFT_SECTION
};

struct SwiftMetadataNote {
  ElfNoteHeader header;
  char name[8]; // "swift6\0" + 1 byte(s) of padding

  MetadataSections sections;
};

/// .note.swift_metadata section stores the table of start/stop symbols for
/// ingestion into the runtime and for static tool access.
// TODO: We should replace addNewDSOImage with something that takes a pointer to
// the table directly, removing the need for the additional global constructor
// that converts the start/stop symbol pointer ranges to start/byte-count,
// before stabilizing any ELF platform ABIs. This will help reduce program
// launch times.
// This will require changes to Swift Testing and the runtime metadata
// registration.
__asm__(".section .note.swift_metadata, \"a\", @note");

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
__attribute__((section(".note.swift_metadata"), used, aligned(4)))
const static SwiftMetadataNote swiftMetadataNote = {
  // header
  .header = {
    .namesz = 7,                               // namesz: "swift6\0"
    .descsz = sizeof(MetadataSections),
    .type = SWIFT_METADATA_SEGMENT_TYPE,
  },
  .name = {
    's', 'w', 'i', 'f', 't', '6', '\0',
    '\0', // padding
  },
  .sections = {
    .version = 5,
    .base = reinterpret_cast<uintptr_t>(&__ehdr_start),
    .unused0 = 0xa1a1a1a1,
    .unused1 = 0xb2b2b2b2,

#define HANDLE_SWIFT_SECTION(elfname, coffname) \
    .elfname = { \
      reinterpret_cast<uintptr_t>(&__start_##elfname), \
      reinterpret_cast<uintptr_t>(&__stop_##elfname), },

#include "MetadataSectionNames.def"

#undef HANDLE_SWIFT_SECTION

  },
};
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END

} // anonymous namespace

namespace {
static swift::MetadataSections sections{};
}

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
__attribute__((__constructor__))
static void swift_image_constructor() {
#define SWIFT_SECTION_RANGE(name) \
  { swiftMetadataNote.sections.name.start, \
    swiftMetadataNote.sections.name.stop - swiftMetadataNote.sections.name.start, }

  ::new(&sections) swift::MetadataSections {
      swiftMetadataNote.sections.version,
      reinterpret_cast<const char*>(swiftMetadataNote.sections.base),

      reinterpret_cast<void*>(swiftMetadataNote.sections.unused0),
      reinterpret_cast<void*>(swiftMetadataNote.sections.unused1),

#define HANDLE_SWIFT_SECTION(elfname, coffname) \
      SWIFT_SECTION_RANGE(elfname),

#include "MetadataSectionNames.def"

#undef HANDLE_SWIFT_SECTION
  };

#undef SWIFT_SECTION_RANGE

  swift_addNewDSOImage(&sections);
}
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END
