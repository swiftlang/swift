//===--- SwiftRT-ELF-WASM.cpp ---------------------------------------------===//
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

#include <cstddef>
#include <new>

#if defined(__ELF__) && __has_include(<elf.h>)
#define EMIT_NOTES 1
#include <elf.h>
#else
#define EMIT_NOTES 0
#endif

#if defined(__ELF__)
extern "C" const char __dso_handle[];
#elif defined(__wasm__)
// NOTE: Multi images in a single process is not yet
// stabilized in WebAssembly toolchain outside of Emscripten.
static constexpr const void *__dso_handle = nullptr;
#endif

#if SWIFT_ENABLE_BACKTRACING
// Drag in a symbol from the backtracer, to force the static linker to include
// the code.
static const void *__backtraceRef __attribute__((used, retain))
  = (const void *)swift::runtime::backtrace::_swift_backtrace_isThunkFunction;
#endif

// Create empty sections to ensure that the start/stop symbols are synthesized
// by the linker.  Otherwise, we may end up with undefined symbol references as
// the linker table section was never constructed.
#if defined(__ELF__)
# define DECLARE_EMPTY_METADATA_SECTION(name, attrs) __asm__("\t.section " #name ",\"" attrs "\"\n");
#elif defined(__wasm__)
# define DECLARE_EMPTY_METADATA_SECTION(name, attrs) __asm__("\t.section " #name ",\"R\",@\n");
#endif

#define BOUNDS_VISIBILITY __attribute__((__visibility__("hidden"), \
                                         __aligned__(1)))

#define DECLARE_BOUNDS(name)                            \
  BOUNDS_VISIBILITY extern const char __start_##name;   \
  BOUNDS_VISIBILITY extern const char __stop_##name;

#if EMIT_NOTES
/// A structure, compatible with the standard ELF note layout, that describes
/// the bounds of a section known to Swift.
///
/// Sections described by these notes can be looked up at runtime using
/// `dl_iterate_phdr()` unless an image's notes have been stripped.
struct SectionNote {
  /// The standard ELF note header.
  ElfW(Nhdr) header;

  /// The name of the ELF note.
  ///
  /// The size of this array must be a multiple of `sizeof(void *)` plus `4` to
  /// ensure correct alignment on 64-bit archs (because `ElfW(Nhdr)` is 12 bytes
  /// long and only 4-byte aligned.)
  char n_name[28];

  /// The "payload" of the note.
  struct Bounds {
    /// The start address of the section.
    const void *start;

    /// The end address of the section.
    const void *end;
  };

  /// The bounds of the section.
  Bounds bounds;
};

#define DECLARE_NOTE(name)                                   \
  __attribute__((section(".note.swift5.section"), used))     \
  static const SectionNote note_##name = {                   \
    {                                                        \
      sizeof(SectionNote::n_name), /* n_namesz */            \
      sizeof(SectionNote::Bounds), /* n_descsz */            \
      0 /* n_type (unused) */                                \
    },                                                       \
    #name,                                                   \
    &__start_##name,                                         \
    &__stop_##name                                           \
  };
#else
#define DECLARE_NOTE(name)
#endif

#define DECLARE_SWIFT_SECTION(name)             \
  DECLARE_EMPTY_METADATA_SECTION(name, "aR")    \
  DECLARE_BOUNDS(name)                          \
  DECLARE_NOTE(name)

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
static swift::MetadataSections sections{};
}

__attribute__((__constructor__))
static void swift_image_constructor() {
#define SWIFT_SECTION_RANGE(name)                                              \
  { reinterpret_cast<uintptr_t>(&__start_##name),                              \
    static_cast<uintptr_t>(&__stop_##name - &__start_##name) }

  ::new (&sections) swift::MetadataSections {
      swift::CurrentSectionMetadataVersion,
      { __dso_handle },

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
