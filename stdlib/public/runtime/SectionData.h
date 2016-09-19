//===-- SectionData.h ------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_RUNTIME_SECTIONDATA_H
#define SWIFT_RUNTIME_SECTIONDATA_H

#if defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#elif defined(__ELF__) || defined(__ANDROID__)
#include <elf.h>
#include <link.h>
#endif

#if defined(_MSC_VER)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif


#if defined(__APPLE__) && defined(__MACH__)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION "__swift2_proto"
#define SWIFT_TYPE_METADATA_SECTION "__swift2_types"
#elif defined(__ELF__)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION ".swift2_protocol_conformances_start"
#define SWIFT_TYPE_METADATA_SECTION ".swift2_type_metadata_start"

#if defined(__linux__)
#define SUPPORTS_STATIC_BINARIES
// Add a declaration for each section
extern const void *__swift2_protocol_conformances_start;
extern const void *__swift2_type_metadata_start;
#endif // __linux__

#elif defined(__CYGWIN__) || defined(_MSC_VER)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION ".sw2prtc"
#define SWIFT_TYPE_METADATA_SECTION ".sw2tymd"
#endif


namespace swift {
  // Common Structure
  struct InspectArgs {
    void (*fnAddImageBlock)(const uint8_t *, size_t);
    const char *sectionName;
#if defined(SUPPORTS_STATIC_BINARIES)
    const void **sectionDataAddr;
#endif
  };

#if defined(__APPLE__) && defined(__MACH__)
  void _swift_initializeCallbacksForSectionData(void (*func)(const mach_header*,
                                                             intptr_t));
  void _swift_readSectionData(const mach_header *mh, InspectArgs *inspectArgs);
#else
  void _swift_initializeCallbacksForSectionData(InspectArgs *inspectArgs);
#endif
}

#endif /* SWIFT_RUNTIME_SECTIONDATA_H */
