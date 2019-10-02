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
/// ELF specific image inspection routines.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_IMAGEINSPECTIONELF_H
#define SWIFT_RUNTIME_IMAGEINSPECTIONELF_H

#define SWIFT_REFLECTION_METADATA_ELF_NOTE_MAGIC_STRING "swift_reflection_metadata_magic_string"

#if defined(__ELF__)

#include "../SwiftShims/Visibility.h"
#include "swift/Basic/RelativePointer.h"
#include <cstdint>
#include <cstddef>

namespace swift {
struct SectionInfo {
  uint64_t size;
  const char *data;
};

static constexpr const uintptr_t CurrentSectionMetadataVersion = 0;

struct MetadataSections {
  const uint32_t version;

  struct Range {
    const RelativeDirectPointer<void> start;
    const RelativeDirectPointer<void> end;

    uintptr_t length() const {
      return (uintptr_t)end.get() - (uintptr_t)start.get();
    }
  };

  const Range swift5_protocols;
  const Range swift5_protocol_conformances;
  const Range swift5_type_metadata;
  const Range swift5_typeref;
  const Range swift5_reflstr;
  const Range swift5_fieldmd;
  const Range swift5_assocty;
  const Range swift5_replace;
  const Range swift5_replac2;
  const Range swift5_builtin;
  const Range swift5_capture;
};

struct MetadataSectionsList {
  MetadataSectionsList *prev, *next;
  const MetadataSections *const sections;
};

// Called by injected constructors when an image is loaded.
SWIFT_RUNTIME_EXPORT
void swift_addNewImage(MetadataSectionsList *addr);

} // namespace swift


#endif // defined(__ELF__)

#endif // SWIFT_RUNTIME_IMAGE_INSPECTION_ELF_H
