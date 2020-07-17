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

static swift::MetadataSections *registered = nullptr;

void record(swift::MetadataSections *sections) {
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
}

SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(const void *addr) {
  // We cast off the const in order to update the linked list
  // data structure. This is safe to do since we don't touch 
  // any other fields.
  swift::MetadataSections *sections =
      static_cast<swift::MetadataSections *>(const_cast<void *>(addr));

  record(sections);

  const auto &protocols_section = sections->swift5_protocols;
  const void *protocols = reinterpret_cast<void *>(protocols_section.start);
  if (protocols_section.length)
    swift::addImageProtocolsBlockCallback(protocols, protocols_section.length);

  const auto &protocol_conformances = sections->swift5_protocol_conformances;
  const void *conformances =
      reinterpret_cast<void *>(protocol_conformances.start);
  if (protocol_conformances.length)
    swift::addImageProtocolConformanceBlockCallback(conformances,
                                             protocol_conformances.length);

  const auto &type_metadata = sections->swift5_type_metadata;
  const void *metadata = reinterpret_cast<void *>(type_metadata.start);
  if (type_metadata.length)
    swift::addImageTypeMetadataRecordBlockCallback(metadata, type_metadata.length);

  const auto &dynamic_replacements = sections->swift5_replace;
  const auto *replacements =
      reinterpret_cast<void *>(dynamic_replacements.start);
  if (dynamic_replacements.length) {
    const auto &dynamic_replacements_some = sections->swift5_replac2;
    const auto *replacements_some =
      reinterpret_cast<void *>(dynamic_replacements_some.start);
    swift::addImageDynamicReplacementBlockCallback(
        replacements, dynamic_replacements.length, replacements_some,
        dynamic_replacements_some.length);
  }
}

void swift::initializeProtocolLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    const swift::MetadataSectionRange &protocols =
      sections->swift5_protocols;
    if (protocols.length)
      addImageProtocolsBlockCallbackUnsafe(
          reinterpret_cast<void *>(protocols.start), protocols.length);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

void swift::initializeProtocolConformanceLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    const swift::MetadataSectionRange &conformances =
        sections->swift5_protocol_conformances;
    if (conformances.length)
      addImageProtocolConformanceBlockCallbackUnsafe(
          reinterpret_cast<void *>(conformances.start), conformances.length);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

void swift::initializeTypeMetadataRecordLookup() {
  const swift::MetadataSections *sections = registered;
  while (true) {
    const swift::MetadataSectionRange &type_metadata =
        sections->swift5_type_metadata;
    if (type_metadata.length)
      addImageTypeMetadataRecordBlockCallbackUnsafe(
          reinterpret_cast<void *>(type_metadata.start), type_metadata.length);

    if (sections->next == registered)
      break;
    sections = sections->next;
  }
}

void swift::initializeDynamicReplacementLookup() {
}

// This is only used for backward deployment hooks, which we currently only support for
// MachO. Add a stub here to make sure it still compiles.
void *swift::lookupSection(const char *segment, const char *section, size_t *outSize) {
  return nullptr;
}

SWIFT_RUNTIME_EXPORT
const swift::MetadataSections *swift_getMetadataSection(size_t index) {
  #ifndef NDEBUG
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
  #else // NDEBUG
  return nullptr;
  #endif // else NDEBUG
}

SWIFT_RUNTIME_EXPORT
const char *swift_getMetadataSectionName(void *metadata_section) {
  #ifndef NDEBUG
  swift::SymbolInfo info;
  if (lookupSymbol(metadata_section, &info)) {
    if (info.fileName) {
      return info.fileName;
    }
  }
  #endif // NDEBUG
  return "";
}

SWIFT_RUNTIME_EXPORT
size_t swift_getMetadataSectionCount() {
  #ifndef NDEBUG
  if (swift::registered == nullptr)
    return 0;

  size_t count = 1;
  for (const auto *current = swift::registered->next;
       current != swift::registered; current = current->next, ++count);

  return count;
  #else // NDEBUG
  return 0;
  #endif // else NDEBUG
}
#endif // !defined(__MACH__)

#endif // SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H