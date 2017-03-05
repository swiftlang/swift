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
//
// ELF specific image inspection routines.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_IMAGE_INSPECTION_ELF_H
#define SWIFT_RUNTIME_IMAGE_INSPECTION_ELF_H

#if defined(__ELF__) || defined(__ANDROID__)

#include "../SwiftShims/Visibility.h"
#include <cstdint>

namespace swift {
  struct SectionInfo {
    uint64_t size;
    const char *data;
  };
}

// Called by injected constructors when a dynamic library is loaded.
SWIFT_RUNTIME_EXPORT
void swift_addNewDSOImage(const void *addr);

#endif // defined(__ELF__) || defined(__ANDROID__)

#endif // SWIFT_RUNTIME_IMAGE_INSPECTION_ELF_H
