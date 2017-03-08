//===--- ImageInspectionStatic.cpp ----------------------------------------===//
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
//
// Implementation of functions to read data sections from static executable.
//
//===----------------------------------------------------------------------===//

// Currently only tested on linux but should work for any ELF platform
#if defined(__ELF__) && defined(__linux__)

#include "ImageInspection.h"
#include <cstring>

// These are defined in swift_sections.S to mark the start of a section with the
// length of the data followed immediately by the section data
struct alignas(uint64_t) Section;
extern const Section protocolConformancesStart asm(".swift2_protocol_conformances_start");
extern const Section typeMetadataStart asm(".swift2_type_metadata_start");

using namespace swift;

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

#endif // defined(__ELF__) && defined(__linux__)
