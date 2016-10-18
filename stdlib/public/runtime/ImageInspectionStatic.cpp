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

// Currently only tested on linux but should work for any ELF platform
#if defined(__ELF__) && defined(__linux__)

// These are defined in swift_sections.S
// Used in swift_sections.S to mark the start of a section
struct SectionInfo {
  uint64_t size;
  const uint8_t data[0];
};
extern const SectionInfo __swift2_protocol_conformances_start;
extern const SectionInfo __swift2_type_metadata_start;


void
swift::initializeProtocolConformanceLookup() {
  addImageProtocolConformanceBlockCallback(
    __swift2_protocol_conformances_start.data,
    __swift2_protocol_conformances_start.size);
}

void
swift::initializeTypeMetadataRecordLookup() {
  addImageTypeMetadataRecordBlockCallback(
    __swift2_type_metadata_start.data,
    __swift2_type_metadata_start.size);
}

// This is called from Errors.cpp when dumping a stack trace entry.
// It could be implemented by parsing the ELF information in the
// executable. For now it returns 0 for error (cant lookup address).
int
swift::lookupSymbol(const void *address, SymbolInfo *info) {
  return 0;
}

#endif
