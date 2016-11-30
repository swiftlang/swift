//===-- ImageInspectionStatic.cpp -------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementation of functions to read data sections from static executable.
//
//===----------------------------------------------------------------------===//

#include "ImageInspection.h"
#include <cstring>

// Currently only tested on linux but should work for any ELF platform
#if defined(__ELF__) && defined(__linux__)

// These are defined in swift_sections.S to mark the start of a section with the
// length of the data followed immediately by the section data
struct alignas(uint64_t) Section;
extern const Section protocolConformancesStart asm(".swift2_protocol_conformances_start");
extern const Section typeMetadataStart asm(".swift2_type_metadata_start");

struct SectionInfo {
  uint64_t size;
  const char *data;
};

static SectionInfo
getSectionInfo(const Section *section) {
  SectionInfo info;
  memcpy(&info.size, section, sizeof(uint64_t));
  info.data = reinterpret_cast<const char *>(section) + sizeof(uint64_t);
  return info;
}

void
swift::initializeProtocolConformanceLookup() {
  auto protocolConformances = getSectionInfo(&protocolConformancesStart);
  addImageProtocolConformanceBlockCallback(protocolConformances.data,
                                           protocolConformances.size);
}

void
swift::initializeTypeMetadataRecordLookup() {
  auto typeMetadata = getSectionInfo(&typeMetadataStart);
  addImageTypeMetadataRecordBlockCallback(typeMetadata.data,
                                          typeMetadata.size);
}

// This is called from Errors.cpp when dumping a stack trace entry.
// It could be implemented by parsing the ELF information in the
// executable. For now it returns 0 for error (cant lookup address).
int
swift::lookupSymbol(const void *address, SymbolInfo *info) {
  return 0;
}

#endif
