//===--- ImageInspectionStatic.cpp - image inspection for static stdlib ---===//
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
/// Implementation of ImageInspection for static stdlib (no dynamic loader
/// present) environments. Assumes that only a single image exists in memory.
///
//===----------------------------------------------------------------------===//

#if defined(__MACH__) && defined(SWIFT_RUNTIME_STATIC_IMAGE_INSPECTION)

#include "ImageInspection.h"
#include "ImageInspectionCommon.h"

extern "C" const char __dso_handle[];

using namespace swift;

#define GET_SECTION_START_AND_SIZE(start, size, _seg, _sec)                    \
  extern void *__s##_seg##_sec __asm("section$start$" _seg "$" _sec);          \
  extern void *__e##_seg##_sec __asm("section$end$" _seg "$" _sec);            \
  start = &__s##_seg##_sec;                                                    \
  size = (char *)&__e##_seg##_sec - (char *)&__s##_seg##_sec;

void swift::initializeProtocolLookup() {
  void *start;
  uintptr_t size;
  GET_SECTION_START_AND_SIZE(start, size, MachOTextSegment,
                             MachOProtocolsSection);
  if (start == nullptr || size == 0)
    return;
  addImageProtocolsBlockCallbackUnsafe(__dso_handle, start, size);
}

void swift::initializeProtocolConformanceLookup() {
  void *start;
  uintptr_t size;
  GET_SECTION_START_AND_SIZE(start, size, MachOTextSegment,
                             MachOProtocolConformancesSection);
  if (start == nullptr || size == 0)
    return;
  addImageProtocolConformanceBlockCallbackUnsafe(__dso_handle, start, size);
}
void swift::initializeTypeMetadataRecordLookup() {
  void *start;
  uintptr_t size;
  GET_SECTION_START_AND_SIZE(start, size, MachOTextSegment,
                             MachOTypeMetadataRecordSection);
  if (start == nullptr || size == 0)
    return;
  addImageTypeMetadataRecordBlockCallbackUnsafe(__dso_handle, start, size);
}

void swift::initializeDynamicReplacementLookup() {
  void *start1;
  uintptr_t size1;
  GET_SECTION_START_AND_SIZE(start1, size1, MachOTextSegment,
                             MachODynamicReplacementSection);
  if (start1 == nullptr || size1 == 0)
    return;
  void *start2;
  uintptr_t size2;
  GET_SECTION_START_AND_SIZE(start2, size2, MachOTextSegment,
                             MachODynamicReplacementSection);
  if (start2 == nullptr || size2 == 0)
    return;
  addImageDynamicReplacementBlockCallback(__dso_handle,
                                          start1, size1, start2, size2);
}
void swift::initializeAccessibleFunctionsLookup() {
  void *start;
  uintptr_t size;
  GET_SECTION_START_AND_SIZE(start, size, MachOTextSegment,
                             MachOAccessibleFunctionsSection);
  if (start != nullptr && size > 0) {
    addImageAccessibleFunctionsBlockCallbackUnsafe(__dso_handle, start, size);
  }
}

#endif // defined(__MACH__) && defined(SWIFT_RUNTIME_STATIC_IMAGE_INSPECTION)
