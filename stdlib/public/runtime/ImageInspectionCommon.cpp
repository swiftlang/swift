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
#include <atomic>
#include <cstdlib>

namespace swift {

static Lazy<ConcurrentReadableArray<swift::MetadataSections *>> registered;

/// Adjust the \c baseAddress field of a metadata sections structure.
///
/// \param sections A pointer to a valid \c swift::MetadataSections structure.
///
/// This function should be called at least once before the structure or its
/// address is passed to code outside this file to ensure that the structure's
/// \c baseAddress field correctly points to the base address of the image it
/// is describing.
static void fixupMetadataSectionBaseAddress(swift::MetadataSections *sections) {
  bool fixupNeeded = false;

#if defined(__ELF__)
  // If the base address was set but the image is an ELF image, it is going to
  // be __dso_handle which is not the value we expect (Dl_info::dli_fbase), so
  // we need to fix it up.
  fixupNeeded = true;
#elif !defined(__MACH__)
  // For non-ELF, non-Apple platforms, if the base address is nullptr, it
  // implies that this image was built against an older version of the runtime
  // that did not capture any value for the base address.
  auto oldBaseAddress = sections->baseAddress.load(std::memory_order_relaxed);
  if (!oldBaseAddress) {
    fixupNeeded = true;
  }
#endif

  if (fixupNeeded) {
    // We need to fix up the base address. We'll need a known-good address in
    // the same image: `sections` itself will work nicely.
    auto symbolInfo = SymbolInfo::lookup(sections);
    if (symbolInfo.has_value() && symbolInfo->getBaseAddress()) {
        sections->baseAddress.store(symbolInfo->getBaseAddress(),
                                    std::memory_order_relaxed);
    }
  }
}
}

SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(swift::MetadataSections *sections) {
#if 0
  // Ensure the base address of the sections structure is correct.
  //
  // Currently disabled because none of the registration functions below
  // actually do anything with the baseAddress field. Instead,
  // swift_enumerateAllMetadataSections() is called by other individual
  // functions, lower in this file, that yield metadata section pointers.
  //
  // If one of these registration functions starts needing the baseAddress
  // field, this call should be enabled and the calls elsewhere in the file can
  // be removed.
  swift::fixupMetadataSectionBaseAddress(sections);
#endif
  auto baseAddress = sections->baseAddress.load(std::memory_order_relaxed);

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
    // Ensure the base address is fixed up before yielding the pointer.
    swift::fixupMetadataSectionBaseAddress(sections);

    // Yield the pointer and (if the callback returns false) break the loop.
    if (!(* body)(sections, context)) {
      return;
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

  if (result) {
    // Ensure the base address is fixed up before returning it.
    swift::fixupMetadataSectionBaseAddress(result);
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

  // fixupMetadataSectionBaseAddress() was already called by
  // swift_getMetadataSection(), presumably on the same thread, so we don't need
  // to call it again here.
  *out_expected = section->baseAddress.load(std::memory_order_relaxed);
}

SWIFT_RUNTIME_EXPORT
size_t swift_getMetadataSectionCount() {
  auto snapshot = swift::registered->snapshot();
  return snapshot.count();
}

#endif // NDEBUG

#endif // !defined(__MACH__)

#endif // SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H
