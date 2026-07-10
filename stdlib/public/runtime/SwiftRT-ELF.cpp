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
# define DECLARE_EMPTY_METADATA_SECTION(name, attrs) __asm__("\t.section " #name ",\"" attrs "\"\n");

#define BOUNDS_VISIBILITY __attribute__((__visibility__("hidden"), \
                                         __aligned__(1)))

#define DECLARE_BOUNDS(name)                            \
  BOUNDS_VISIBILITY extern const char __start_##name;   \
  BOUNDS_VISIBILITY extern const char __stop_##name;

#define DECLARE_SWIFT_SECTION(name)             \
  DECLARE_EMPTY_METADATA_SECTION(name, "aR")    \
  DECLARE_BOUNDS(name)

// These may or may not be present, depending on compiler switches; it's
// worth calling them out as a result.
#define DECLARE_SWIFT_REFLECTION_SECTION(name)  \
  DECLARE_SWIFT_SECTION(name)

#define DECLARE_SWIFT_REFLECTION_SECTION_NO_RETAIN(name) \
  DECLARE_EMPTY_METADATA_SECTION(name, "a")              \
  DECLARE_BOUNDS(name)

extern "C" {
DECLARE_SWIFT_SECTION(swift5_protocols)
DECLARE_SWIFT_SECTION(swift5_protocol_conformances)
DECLARE_SWIFT_SECTION(swift5_type_metadata)

DECLARE_SWIFT_REFLECTION_SECTION(swift5_fieldmd)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_builtin)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_assocty)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_capture)
DECLARE_SWIFT_REFLECTION_SECTION_NO_RETAIN(swift5_reflstr)
DECLARE_SWIFT_REFLECTION_SECTION_NO_RETAIN(swift5_typeref)
DECLARE_SWIFT_REFLECTION_SECTION(swift5_mpenum)

DECLARE_SWIFT_SECTION(swift5_replace)
DECLARE_SWIFT_SECTION(swift5_replac2)
DECLARE_SWIFT_SECTION(swift5_accessible_functions)
DECLARE_SWIFT_SECTION(swift5_runtime_attributes)

DECLARE_SWIFT_SECTION(swift5_tests)
}

#undef DECLARE_SWIFT_SECTION
#undef DECLARE_SWIFT_REFLECTION_SECTION

namespace {
static swift::MetadataSections sections{};
}

// Statically initialized read-only table identifying Swift reflection metadata
// sections. Exported by the public symbol containing the start/stop pointer
// bounds to each section.
//
// Unlike the `sections` table above, the data is initialized by the linker,
// making the data valid both on disk and when loaded, so no need for a loader,
// relocations, or a constructor function

struct SwiftReflectionSectionBounds {
  const void *start;
  const void *stop;
};

#define SWIFT_REFLECTION_SECTIONS_VERSION 1u

// This ordering matches the ReflectionInfo struct layout in
// `ReflectionContext.h`.
//
// NOTE! Bump the version number if new members are added or if they are
// re-ordered.
struct SwiftReflectionSections {
  __swift_uintptr_t version;

  SwiftReflectionSectionBounds swift5_fieldmd;
  SwiftReflectionSectionBounds swift5_assocty;
  SwiftReflectionSectionBounds swift5_builtin;
  SwiftReflectionSectionBounds swift5_capture;
  SwiftReflectionSectionBounds swift5_typeref;
  SwiftReflectionSectionBounds swift5_reflstr;
  SwiftReflectionSectionBounds swift5_protocol_conformances;
  SwiftReflectionSectionBounds swift5_mpenum;
};

#define SWIFT_REFLECTION_SECTION_BOUNDS(name)                                  \
  {static_cast<const void *>(&__start_##name),                                 \
   static_cast<const void *>(&__stop_##name)}

extern "C" __attribute__((__used__, __visibility__("default")))
const SwiftReflectionSections __swift5_reflection_sections = {
    SWIFT_REFLECTION_SECTIONS_VERSION,
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_fieldmd),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_assocty),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_builtin),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_capture),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_typeref),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_reflstr),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_protocol_conformances),
    SWIFT_REFLECTION_SECTION_BOUNDS(swift5_mpenum),
};

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
__attribute__((__constructor__))
static void swift_image_constructor() {
#define SWIFT_SECTION_RANGE(name)                                              \
  { reinterpret_cast<uintptr_t>(&__start_##name),                              \
    static_cast<uintptr_t>(&__stop_##name - &__start_##name) }

  const void *baseAddress = nullptr;
  if (&__ehdr_start != nullptr) {
    baseAddress = __ehdr_start;
  }

  ::new (&sections) swift::MetadataSections {
      swift::CurrentSectionMetadataVersion,
      baseAddress,

      nullptr,
      nullptr,

      SWIFT_SECTION_RANGE(swift5_protocols),
      SWIFT_SECTION_RANGE(swift5_protocol_conformances),
      SWIFT_SECTION_RANGE(swift5_type_metadata),

      SWIFT_SECTION_RANGE(swift5_typeref),
      SWIFT_SECTION_RANGE(swift5_reflstr),
      SWIFT_SECTION_RANGE(swift5_fieldmd),
      SWIFT_SECTION_RANGE(swift5_assocty),
      SWIFT_SECTION_RANGE(swift5_replace),
      SWIFT_SECTION_RANGE(swift5_replac2),
      SWIFT_SECTION_RANGE(swift5_builtin),
      SWIFT_SECTION_RANGE(swift5_capture),
      SWIFT_SECTION_RANGE(swift5_mpenum),
      SWIFT_SECTION_RANGE(swift5_accessible_functions),
      SWIFT_SECTION_RANGE(swift5_runtime_attributes),
      SWIFT_SECTION_RANGE(swift5_tests),
  };

#undef SWIFT_SECTION_RANGE

  swift_addNewDSOImage(&sections);
}
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END
