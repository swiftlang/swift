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
#include <cstdint>
#include <cstddef>

namespace swift {
struct SectionInfo {
  uint64_t size;
  const char *data;
};

static constexpr const uintptr_t CurrentSectionMetadataVersion = 1;
} // namespace swift

// Called by injected constructors when a dynamic library is loaded.
SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(const void *addr);

#endif // defined(__ELF__)

#endif // SWIFT_RUNTIME_IMAGE_INSPECTION_ELF_H
