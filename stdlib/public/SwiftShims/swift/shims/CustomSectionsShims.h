//===--- CustomSectionsShims.h - dyld access for back deployment -*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Header-only dyld wrappers for the back-deployed copy of
/// `customSectionContents(named:)`, which runs on Apple OS releases whose Swift
/// runtime predates `swift_enumerateCustomSections`. Everything here is inline
/// so that the client binary carries it; nothing is exported from libswiftCore.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_CUSTOMSECTIONSSHIMS_H
#define SWIFT_STDLIB_SHIMS_CUSTOMSECTIONSSHIMS_H

#include "SwiftStddef.h"
#include "SwiftStdint.h"

#if defined(__APPLE__) && defined(__MACH__)

#ifdef __cplusplus
extern "C" {
#endif

// The dyld and libmacho prototypes this file needs, spelled exactly as
// <mach-o/dyld.h> and <mach-o/getsect.h> spell them, so that a translation
// unit including both sees compatible declarations.
struct mach_header;
struct mach_header_64;
__swift_uint32_t _dyld_image_count(void);
const struct mach_header *_Null_unspecified
_dyld_get_image_header(__swift_uint32_t image_index);
#if __LP64__
__swift_uint8_t *_Null_unspecified getsectiondata(
    const struct mach_header_64 *_Null_unspecified mhp,
    const char *_Null_unspecified segname,
    const char *_Null_unspecified sectname,
    unsigned long *_Null_unspecified size);
#else
__swift_uint8_t *_Null_unspecified getsectiondata(
    const struct mach_header *_Null_unspecified mhp,
    const char *_Null_unspecified segname,
    const char *_Null_unspecified sectname,
    unsigned long *_Null_unspecified size);
#endif

static inline __swift_uint32_t _swift_stdlib_customSections_imageCount(void) {
  return _dyld_image_count();
}

static inline const void *_Nullable
_swift_stdlib_customSections_imageHeader(__swift_uint32_t index) {
  return _dyld_get_image_header(index);
}

static inline const void *_Nullable _swift_stdlib_customSections_sectionData(
    const void *_Nonnull header, const char *_Nonnull segment,
    const char *_Nonnull section, __swift_size_t *_Nonnull size) {
  unsigned long sectionSize = 0;
#if __LP64__
  const __swift_uint8_t *data = getsectiondata(
      (const struct mach_header_64 *)header, segment, section, &sectionSize);
#else
  const __swift_uint8_t *data = getsectiondata(
      (const struct mach_header *)header, segment, section, &sectionSize);
#endif
  *size = (__swift_size_t)sectionSize;
  return data;
}

#ifdef __cplusplus
} // extern "C"
#endif

#endif // defined(__APPLE__) && defined(__MACH__)

#endif // SWIFT_STDLIB_SHIMS_CUSTOMSECTIONSSHIMS_H
