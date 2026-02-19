//===--- SwiftRT-ELF-Testing.cpp ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ImageInspectionCommon.h"
#include "swift/Runtime/Config.h"

#if defined(__ELF__)
extern "C" const char __ehdr_start[] __attribute__((__weak__));
extern "C" const char __start_swift5_tests[] __attribute__((weak));
extern "C" const char __stop_swift5_tests[] __attribute__((weak));

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
__attribute__((__constructor__(101)))
static void _swift_elf_discoverTestContent(void) {
  if (SWIFT_LIKELY(&__start_swift5_tests == nullptr)) {
    // Early exit in the common case where an image does not have test content.
    return;
  }

  swift::TestContentSectionBounds sectionBounds {};
  if (&__ehdr_start != nullptr) {
    sectionBounds.baseAddress = __ehdr_start;
  }
  sectionBounds.start = reinterpret_cast<uintptr_t>(__start_swift5_tests);
  sectionBounds.length = __stop_swift5_tests - __start_swift5_tests;
  swift_elf_registerTestContent(&sectionBounds);
}
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END
#endif
