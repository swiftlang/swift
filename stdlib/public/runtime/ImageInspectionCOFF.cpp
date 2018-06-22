//===--- ImageInspectionWin32.cpp - Win32 image inspection ----------------===//
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

#if !defined(__ELF__) && !defined(__MACH__)

#include "ImageInspection.h"
#include "ImageInspectionCOFF.h"
#include "swift/Runtime/Metadata.h"

#if defined(__CYGWIN__)
#include <dlfcn.h>
#endif
#include <memory>

using namespace swift;

namespace {
static const swift::MetadataSections *registered = nullptr;

void record(const swift::MetadataSections *sections) {
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

  // Returns true if all bytes in the given memory range are 0.
  bool areAllBytesZero(const char *start, std::size_t length) {
    for (auto p = start; p < start + length; p++) {
      if (*p != 0) {
        return false;
      }
    }

    return true;
  }

  // Checks whether the beginning and the end of the given section range is
  // padding with 0 bytes and adjusts the section range such that the padding
  // bytes are removed from the range. We do this because the MSVC linker adds 0
  // padding bytes in incremental linking mode (which is on by default for debug
  // builds).
  swift::MetadataSections::Range adjustedRange(
                                          swift::MetadataSections::Range range,
                                          std::size_t recordStride,
                                          std::size_t alignment) {
    std::size_t space = range.length;
    void *start = reinterpret_cast<void *>(range.start);
    const char *alignedStart = reinterpret_cast<const char *>(std::align(
                                          alignment, recordStride, start, space));
    const char *end = reinterpret_cast<char *>(range.start + range.length);
    const char *alignedEnd = alignedStart + (space / recordStride) * recordStride;
    bool done = false;

    if (space == 0) {
      return range;
    }

    // Skip 0 padding at the beginning of the section
    const char *adjStart = alignedStart;
    while (adjStart < alignedEnd) {
      if (!areAllBytesZero(adjStart, recordStride)) {
        break;
      }

      adjStart += recordStride;
    }

    // Skip 0 padding at the end of the section
    const char *adjEnd = alignedEnd;
    while (adjEnd > adjStart) {
      if (!areAllBytesZero(adjEnd - recordStride, recordStride)) {
        break;
      }

      adjEnd -= recordStride;
    }

    return {reinterpret_cast<uintptr_t>(adjStart),
        static_cast<std::size_t>(adjEnd - adjStart)};
  }

  void registerImageProtocolsSection(
                                     swift::MetadataSections::Range section) {
    auto adjSection = adjustedRange(section, sizeof(ProtocolRecord), 4);

    if (adjSection.length) {
      addImageProtocolsBlockCallback(
          reinterpret_cast<void *>(adjSection.start), adjSection.length);
    }
  }

  void registerImageProtocolConformanceSection(
                                      swift::MetadataSections::Range section) {
    auto adjSection = adjustedRange(section, sizeof(ProtocolConformanceRecord), 4);

    if (adjSection.length) {
      addImageProtocolConformanceBlockCallback(
          reinterpret_cast<void *>(adjSection.start), adjSection.length);
    }
  }

  void registerImageTypeMetadataRecordSection(
                                      swift::MetadataSections::Range section) {
    auto adjSection = adjustedRange(section, sizeof(TypeMetadataRecord), 4);

    if (adjSection.length) {
      addImageTypeMetadataRecordBlockCallback(
          reinterpret_cast<void *>(adjSection.start), adjSection.length);
    }
  }

  void registerImageTypeFieldDescriptorSection(
                                      swift::MetadataSections::Range section) {
    auto adjSection = adjustedRange(section,
                                sizeof(swift::reflection::FieldDescriptor), 4);

    if (adjSection.length) {
      addImageTypeFieldDescriptorBlockCallback(
          reinterpret_cast<void *>(adjSection.start), adjSection.length);
    }
  }
}

void swift::initializeProtocolLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    registerImageProtocolsSection(sections->swift5_protocols);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

void swift::initializeProtocolConformanceLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    registerImageProtocolConformanceSection(
        sections->swift5_protocol_conformances);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

void swift::initializeTypeMetadataRecordLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    registerImageTypeMetadataRecordSection(sections->swift5_type_metadata);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

void swift::initializeTypeFieldLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    registerImageTypeFieldDescriptorSection(sections->swift5_fieldmd);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(const void *addr) {
  const swift::MetadataSections *sections =
      static_cast<const swift::MetadataSections *>(addr);

  record(sections);

  registerImageProtocolsSection(sections->swift5_protocols);
  registerImageProtocolConformanceSection(sections->swift5_protocol_conformances);
  registerImageTypeMetadataRecordSection(sections->swift5_type_metadata);
}

int swift::lookupSymbol(const void *address, SymbolInfo *info) {
#if defined(__CYGWIN__)
  Dl_info dlinfo;
  if (dladdr(address, &dlinfo) == 0) {
    return 0;
  }

  info->fileName = dlinfo.dli_fname;
  info->baseAddress = dlinfo.dli_fbase;
  info->symbolName = dli_info.dli_sname;
  info->symbolAddress = dli_saddr;
  return 1;
#else
  return 0;
#endif // __CYGWIN__
}

// This is only used for backward deployment hooks, which we currently only support for
// MachO. Add a stub here to make sure it still compiles.
void *swift::lookupSection(const char *segment, const char *section, size_t *outSize) {
  return nullptr;
}

#endif // !defined(__ELF__) && !defined(__MACH__)
