//===--- ImageInspectionELF.h -----------------------------------*- C++ -*-===//
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
/// COFF specific image inspection routines.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_IMAGEINSPECTIONCOFF_H
#define SWIFT_RUNTIME_IMAGEINSPECTIONCOFF_H

#if !defined(__ELF__) && !defined(__MACH__)

#include "../SwiftShims/Visibility.h"
#include <cstdint>
#include <cstddef>

namespace swift {
struct SectionInfo {
  uint64_t size;
  const char *data;
};

static constexpr const uintptr_t CurrentSectionMetadataVersion = 1;

struct MetadataSections {
  uintptr_t version;
  uintptr_t reserved;

  mutable const MetadataSections *next;
  mutable const MetadataSections *prev;

  struct Range {
    uintptr_t start;
    size_t length;
  };

  Range swift4_protocols;
  Range swift4_protocol_conformances;
  Range swift4_type_metadata;
  Range swift4_typeref;
  Range swift4_reflstr;
  Range swift4_fieldmd;
  Range swift4_assocty;
};
} // namespace swift

// Called by injected constructors when a dynamic library is loaded.
SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(const void *addr);

#endif // !defined(__ELF__) && !defined(__MACH__)

#endif // SWIFT_RUNTIME_IMAGEINSPECTIONCOFF_H
