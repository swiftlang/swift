//===--- ImageInspectionCommon.h - Image inspection routines -----*- C++ -*-===//
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
/// This file unifies common ELF and COFF image inspection routines.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H
#define SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H

#if defined(__MACH__)

/// The Mach-O section name for the section containing protocol descriptor
/// references. This lives within SEG_TEXT.
#define MachOProtocolsSection "__swift5_protos"
/// The Mach-O section name for the section containing protocol conformances.
/// This lives within SEG_TEXT.
#define MachOProtocolConformancesSection "__swift5_proto"
/// The Mach-O section name for the section containing copyable type references.
/// This lives within SEG_TEXT.
#define MachOTypeMetadataRecordSection "__swift5_types"
/// The Mach-O section name for the section containing additional type references.
/// This lives within SEG_TEXT.
#define MachOExtraTypeMetadataRecordSection "__swift5_types2"
/// The Mach-O section name for the section containing dynamic replacements.
/// This lives within SEG_TEXT.
#define MachODynamicReplacementSection "__swift5_replace"
#define MachODynamicReplacementSomeSection "__swift5_replac2"
/// The Mach-O section name for the section containing accessible functions.
/// This lives within SEG_TEXT.
#define MachOAccessibleFunctionsSection "__swift5_acfuncs"

#define MachOTextSegment "__TEXT"

#else

#include "swift/shims/Visibility.h"
#include <cstdint>
#include <cstddef>

namespace swift {
struct MetadataSections;
static constexpr const uintptr_t CurrentSectionMetadataVersion = 4;
}

struct SectionInfo {
  uint64_t size;
  const char *data;
};

/// Called by injected constructors when a dynamic library is loaded.
///
/// \param sections A structure describing the metadata sections in the
///     newly-loaded image.
///
/// \warning The runtime keeps a reference to \a sections and may mutate it, so
///   it \em must be mutable and long-lived (that is, statically or dynamically
///   allocated.) The effect of passing a pointer to a local value is undefined.
SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(struct swift::MetadataSections *sections);

/// Enumerate all metadata sections in the current process that are known to the
/// Swift runtime.
///
/// \param body A function to invoke once per metadata sections structure.
///   If this function returns \c false, enumeration is stopped.
/// \param context An additional context pointer to pass to \a body.
///
/// On Mach-O-based platforms (i.e. Apple platforms), this function is
/// unavailable. On those platforms, use dyld API to enumerate loaded images and
/// their corresponding metadata sections.
SWIFT_RUNTIME_EXPORT SWIFT_WEAK_IMPORT
void swift_enumerateAllMetadataSections(
  bool (* body)(const swift::MetadataSections *sections, void *context),
  void *context
);

#ifndef NDEBUG

SWIFT_RUNTIME_EXPORT
const char *
swift_getMetadataSectionName(const struct swift::MetadataSections *section);

SWIFT_RUNTIME_EXPORT
void swift_getMetadataSectionBaseAddress(
  const struct swift::MetadataSections *section,
  void const **out_actual, void const **out_expected);

SWIFT_RUNTIME_EXPORT
size_t swift_getMetadataSectionCount();

#endif // NDEBUG

#endif // !defined(__MACH__)

#endif // SWIFT_RUNTIME_IMAGEINSPECTIONCOMMON_H
