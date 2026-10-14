//===--- ImageInspectionCommon.cpp - Image inspection routines --*- C++ -*-===//
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
///
/// \file
///
/// This file unifies common ELF and COFF image inspection routines
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H
#define SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H

#if !defined(__MACH__)

#include "swift/shims/Visibility.h"
#include "swift/shims/MetadataSections.h"
#include "ImageInspection.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"

#include <algorithm>
#include <cstdlib>
#include <cstring>

namespace swift {
  static Lazy<ConcurrentReadableArray<swift::MetadataSections *>> registered;
}

SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(swift::MetadataSections *sections) {
#if defined(__ELF__)
  if (!sections->baseAddress || sections->version <= 4) {
    // The base address was either unavailable at link time or is set to the
    // wrong value and will need to be recomputed. We can use the address of the
    // sections structure and derive the base address from there.
    if (auto info = swift::SymbolInfo::lookup(sections)) {
      sections->baseAddress = info->getBaseAddress();
    }
  }
#endif
  auto baseAddress = sections->baseAddress;

  const auto &protocols_section = sections->swift5_protocols;
  const void *protocols = reinterpret_cast<void *>(protocols_section.start);
  if (protocols_section.length)
    swift::addImageProtocolsBlockCallback(baseAddress,
                                          protocols, protocols_section.length);

  const auto &protocol_conformances = sections->swift5_protocol_conformances;
  const void *conformances =
      reinterpret_cast<void *>(protocol_conformances.start);
  if (protocol_conformances.length)
    swift::addImageProtocolConformanceBlockCallback(baseAddress, conformances,
                                             protocol_conformances.length);

  const auto &type_metadata = sections->swift5_type_metadata;
  const void *metadata = reinterpret_cast<void *>(type_metadata.start);
  if (type_metadata.length)
    swift::addImageTypeMetadataRecordBlockCallback(baseAddress,
                                                   metadata,
                                                   type_metadata.length);

  const auto &dynamic_replacements = sections->swift5_replace;
  const auto *replacements =
      reinterpret_cast<void *>(dynamic_replacements.start);
  if (dynamic_replacements.length) {
    const auto &dynamic_replacements_some = sections->swift5_replac2;
    const auto *replacements_some =
      reinterpret_cast<void *>(dynamic_replacements_some.start);
    swift::addImageDynamicReplacementBlockCallback(baseAddress,
        replacements, dynamic_replacements.length, replacements_some,
        dynamic_replacements_some.length);
  }

  const auto &accessible_funcs_section = sections->swift5_accessible_functions;
  const void *functions =
      reinterpret_cast<void *>(accessible_funcs_section.start);
  if (accessible_funcs_section.length) {
    swift::addImageAccessibleFunctionsBlockCallback(
        baseAddress, functions, accessible_funcs_section.length);
  }

  // Register this section for future enumeration by clients. This should occur
  // after this function has done all other relevant work to avoid a race
  // condition when someone calls swift_enumerateAllMetadataSections() on
  // another thread.
  swift::registered->push_back(sections);
}

SWIFT_RUNTIME_EXPORT
void swift_enumerateAllMetadataSections(
  bool (* body)(const swift::MetadataSections *sections, void *context),
  void *context
) {
  auto snapshot = swift::registered->snapshot();
  for (swift::MetadataSections *sections : snapshot) {
    // Yield the pointer and (if the callback returns false) break the loop.
    if (!(* body)(sections, context)) {
      return;
    }
  }
}

/// Resolve one field of a SectionIndexEntry: a 32-bit offset relative to the
/// field's own address.
static const char *resolveSectionIndexField(const __swift_int32_t *field) {
  return reinterpret_cast<const char *>(field) + *field;
}

SWIFT_RUNTIME_EXPORT
void swift_enumerateCustomSections(const char *sectionName,
                                   bool (*body)(const void *start, size_t size,
                                                void *context),
                                   void *context) {
  if (!sectionName)
    return;

  // On COFF the attribute spelling carries a "$" group suffix (".myplugs$B")
  // that the linker folds away; match on the part before it. Elsewhere this
  // is the whole name.
  size_t nameLength = strcspn(sectionName, "$");

  auto snapshot = swift::registered->snapshot();
  for (const swift::MetadataSections *sections : snapshot) {
    // Images built by an older toolchain register a shorter structure that
    // ends before the index field.
    if (sections->version < 6)
      continue;

    const auto &index = sections->swift5_sections;
    auto *entry =
        reinterpret_cast<const swift::SectionIndexEntry *>(index.start);
    auto *end = entry + index.length / sizeof(swift::SectionIndexEntry);
    for (; entry < end; ++entry) {
      const char *name = resolveSectionIndexField(&entry->name);
      if (strncmp(name, sectionName, nameLength) != 0 ||
          name[nameLength] != '\0')
        continue;

      const char *start = resolveSectionIndexField(&entry->start);
      const char *stop = resolveSectionIndexField(&entry->stop);

#if defined(_WIN32)
      // On COFF the entry's start is a marker the compiler placed in the
      // section's "$A" group; its first word is the marker's own size and the
      // records begin right after it.
      if (stop - start >= static_cast<ptrdiff_t>(sizeof(uintptr_t))) {
        uintptr_t markerSize = *reinterpret_cast<const uintptr_t *>(start);
        if (markerSize >= sizeof(uintptr_t) &&
            markerSize <= static_cast<uintptr_t>(stop - start))
          start += markerSize;
        else
          stop = start;
      }
#endif

      // Every entry for one name in one image describes the same bounds (a
      // linker that does not fold the COMDAT groups, such as wasm-ld, leaves
      // duplicates), so the first one is this image's answer.
      if (stop > start &&
          !(*body)(start, static_cast<size_t>(stop - start), context))
        return;
      break;
    }
  }
}

void swift::initializeProtocolLookup() {
}

void swift::initializeProtocolConformanceLookup() {
}

void swift::initializeTypeMetadataRecordLookup() {
}

void swift::initializeDynamicReplacementLookup() {
}

void swift::initializeAccessibleFunctionsLookup() {
}

#ifndef NDEBUG

SWIFT_RUNTIME_EXPORT
const swift::MetadataSections *swift_getMetadataSection(size_t index) {
  swift::MetadataSections *result = nullptr;

  auto snapshot = swift::registered->snapshot();
  if (index < snapshot.count()) {
    result = snapshot[index];
  }

  return result;
}

SWIFT_RUNTIME_EXPORT
const char *
swift_getMetadataSectionName(const swift::MetadataSections *section) {
  if (auto info = swift::SymbolInfo::lookup(section)) {
    if (info->getFilename()) {
      return info->getFilename();
    }
  }
  return "";
}

SWIFT_RUNTIME_EXPORT
void swift_getMetadataSectionBaseAddress(const swift::MetadataSections *section,
                                         void const **out_actual,
                                         void const **out_expected) {
  if (auto info = swift::SymbolInfo::lookup(section)) {
    *out_actual = info->getBaseAddress();
  } else {
    *out_actual = nullptr;
  }
  *out_expected = section->baseAddress;
}

SWIFT_RUNTIME_EXPORT
size_t swift_getMetadataSectionCount() {
  auto snapshot = swift::registered->snapshot();
  return snapshot.count();
}

#endif // NDEBUG

#endif // !defined(__MACH__)

#endif // SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H
