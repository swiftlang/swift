//===-- ImageInspectionInit.cpp -------------------------------------------===//
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
// This file along with swift_sections.S is prepended to each shared library
// on an ELF target which contains protocol and metadata sections.
//
//===----------------------------------------------------------------------===//

#if defined(__ELF__) || defined(__ANDROID__)

#include "ImageInspection.h"
#include <memory>

// This is called at startup and by each shared object as it is dlopen()'d to
// allow the section data for the object to be loaded.
__attribute__((constructor))
static void sectionDataInit() {
  void *addr = reinterpret_cast<void *>(std::addressof(sectionDataInit));
  swift_addNewDSOImage(addr);
}

#endif
