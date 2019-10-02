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

#define FOR_EACH_SWIFT_SECTION(do) \
  do(swift5_protocols) \
  do(swift5_protocol_conformances) \
  do(swift5_type_metadata) \
  do(swift5_typeref) \
  do(swift5_reflstr) \
  do(swift5_fieldmd) \
  do(swift5_assocty) \
  do(swift5_replace) \
  do(swift5_replac2) \
  do(swift5_builtin) \
  do(swift5_capture)

// Create empty sections to ensure that the start/stop symbols are synthesized
// by the linker.  Otherwise, we may end up with undefined symbol references as
// the linker table section was never constructed.

struct Section;

#define DECLARE_SWIFT_SECTION(name) \
  __asm__("\t.section " #name ",\"a\"\n");

extern "C" {
  FOR_EACH_SWIFT_SECTION(DECLARE_SWIFT_SECTION)
}

#undef DECLARE_SWIFT_SECTION

__attribute__((visibility("hidden")))
extern "C" swift::MetadataSections __swift_metadataSections;

#define PREFIX 

// Define `__swift_metadataSections using assembly language, because C++
// constant expressions won't let us subtract global pointers.
__asm__(
"\t.text\n"
"\t.p2align 4\n"
PREFIX "__swift_metadataSections:\n"
"\t.long 0\n"
#define DEFINE_METADATA_SECTION_BOUNDS(section) \
  "\t.long " PREFIX "__start_" #section " - .\n" \
  "\t.long " PREFIX "__stop_" #section " - .\n" \

FOR_EACH_SWIFT_SECTION(DEFINE_METADATA_SECTION_BOUNDS)
#undef DEFINE_METADATA_SECTION_BOUNDS
);

static swift::MetadataSectionsList metadataSectionList = {
  nullptr, nullptr,
  &__swift_metadataSections,
};

__attribute__((__constructor__))
static void swift_image_constructor() {
  swift_addNewImage(&metadataSectionList);
}

__asm__(".section \".note.swift_reflection_metadata\", \"aw\"");

static __attribute__((__used__))
__attribute__((__section__(".note.swift_reflection_metadata")))
__attribute__((__aligned__(1)))
struct {
  const char MagicString[sizeof(SWIFT_REFLECTION_METADATA_ELF_NOTE_MAGIC_STRING)];
  const swift::MetadataSections *Sections;
} __attribute__((__packed__))
Note = {SWIFT_REFLECTION_METADATA_ELF_NOTE_MAGIC_STRING, &__swift_metadataSections};
