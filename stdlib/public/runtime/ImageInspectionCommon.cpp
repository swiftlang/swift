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

#include "../SwiftShims/Visibility.h"
#include "../SwiftShims/MetadataSections.h"
#include "ImageInspection.h"


namespace swift {

#ifndef NDEBUG
static swift::MetadataSections *registered = nullptr;

static void record(swift::MetadataSections *sections) {
  if (registered == nullptr) {
    registered = sections;
    sections->next = sections->prev = sections;
  } else {
    registered->prev->next = sections;
    sections->next = registered;
    sections->prev = registered->prev;
    registered->prev = sections;
  }
}
#endif

static const void *
getMetadataSectionBaseAddress(swift::MetadataSections *sections) {
  // If the base address was not set by the caller of swift_addNewDSOImage()
  // then we can assume that the caller was built against an older version of
  // the runtime that did not capture a value for this field. Currently nothing
  // is actively using the image's base address outside of tests that are built
  // with the runtime/stdlib, so there's no need to try to fix up the value. If
  // something in the runtime starts using it, we will want to either:
  // 1. Resolve the address from a known-good address like swift5_protocols when
  //    the image is first loaded (in this function);
  // 1. Resolve the address from a known-good address like swift5_protocols when
  //    the address is first used (and atomically swap the address back so we
  //    don't incur the cost of lookupSymbol() each time we need it; or
  // 3. Introduce an ABI-breaking change so that all binaries are rebuilt and
  //    start supplying a value for this field.

#ifndef NDEBUG
#if defined(__ELF__)
  // If the base address was set but the image is an ELF image, it is going to
  // be __dso_handle which is not the value we expect (Dl_info::dli_fbase), so
  // we need to fix it up. Since the base address is currently unused by the
  // runtime outside tests, we don't normally do this work.
  if (auto baseAddress = sections->baseAddress) {
    swift::SymbolInfo symbolInfo;
    if (lookupSymbol(baseAddress, &symbolInfo) && symbolInfo.baseAddress) {
      sections->baseAddress = symbolInfo.baseAddress;
    }
  }
#endif
#endif

  return sections->baseAddress;
}
}

SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(swift::MetadataSections *sections) {
#ifndef NDEBUG
  record(sections);
#endif

  auto baseAddress = swift::getMetadataSectionBaseAddress(sections);

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
  if (accessible_funcs_section.length)
    swift::addImageAccessibleFunctionsBlockCallback(
        baseAddress, functions, accessible_funcs_section.length);
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
  if (swift::registered == nullptr) {
    return nullptr;
  }

  auto selected = swift::registered;
  while (index > 0) {
    selected = selected->next;
    if (selected == swift::registered) {
      return nullptr;
    }
    --index;
  }
  return selected;
}

SWIFT_RUNTIME_EXPORT
const char *
swift_getMetadataSectionName(const swift::MetadataSections *section) {
  swift::SymbolInfo info;
  if (lookupSymbol(section, &info)) {
    if (info.fileName) {
      return info.fileName;
    }
  }
  return "";
}

SWIFT_RUNTIME_EXPORT
void swift_getMetadataSectionBaseAddress(const swift::MetadataSections *section,
                                         void const **out_actual,
                                         void const **out_expected) {
  swift::SymbolInfo info;
  if (lookupSymbol(section, &info)) {
    *out_actual = info.baseAddress;
  } else {
    *out_actual = nullptr;
  }

  *out_expected = section->baseAddress;
}

SWIFT_RUNTIME_EXPORT
size_t swift_getMetadataSectionCount() {
  if (swift::registered == nullptr)
    return 0;

  size_t count = 1;
  for (const auto *current = swift::registered->next;
       current != swift::registered; current = current->next, ++count);

  return count;
}

#endif // NDEBUG

#endif // !defined(__MACH__)

#endif // SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H
