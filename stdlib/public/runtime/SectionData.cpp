//===-- SectionData.cpp -----------------------------------------*- C++ -*-===//
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
//
// Implementation of functions to read data sections from dynamic executable.
//
//===----------------------------------------------------------------------===//

#include <cassert>
#include "SectionData.h"

using namespace swift;

#if defined(__APPLE__) && defined(__MACH__)
void
swift::_swift_readSectionData(const mach_header *mh,
                              InspectArgs *inspectArgs) {
#ifdef __LP64__
  using mach_header_platform = mach_header_64;
  assert(mh->magic == MH_MAGIC_64 && "loaded non-64-bit image?!");
#else
  using mach_header_platform = mach_header;
#endif

  // Look for a named section.
  unsigned long dataSize = 0;
  const uint8_t *data =
    getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                   SEG_TEXT, inspectArgs->sectionName,
                   &dataSize);

  if (data) {
    inspectArgs->fnAddImageBlock(data, dataSize);
  }
}

void
swift::_swift_initializeCallbacksForSectionData(InspectArgs *inspectArgs) {
  // Install our dyld callback.
  // Dyld will invoke this on our behalf for all images that have already
  // been loaded.
  _dyld_register_func_for_add_image(inspectArgs->func_for_add_image);
}

#elif defined(__ELF__) || defined(__ANDROID__)

static int
_addImageSectionData(struct dl_phdr_info *info, size_t size, void *data) {
  // inspectArgs contains addImage*Block function and the section name
  InspectArgs *inspectArgs = reinterpret_cast<InspectArgs *>(data);
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0') {
    handle = dlopen(nullptr, RTLD_LAZY);
  } else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);

  if (!handle) {
    // Not a shared library.
    return 0;
  }

  auto imageBlock = reinterpret_cast<const uint8_t*>(
      dlsym(handle, inspectArgs->sectionName));

  if (imageBlock) {
    // Extract the size of the image data from the head of the section
    auto imageBlockSize = *reinterpret_cast<const uint64_t*>(imageBlock);
    imageBlock += sizeof(imageBlockSize);
    inspectArgs->fnAddImageBlock(imageBlock, imageBlockSize);
  }
  dlclose(handle);
  return 0;
}

void
swift::_swift_initializeCallbacksForSectionData(InspectArgs *inspectArgs) {
  // Search the loaded dls. Unlike the above, this only searches the already
  // loaded ones.
  // FIXME: Find a way to have this continue to happen after.
  // rdar://problem/19045112
  dl_iterate_phdr(_addImageSectionData, inspectArgs);
}

#elif defined(__CYGWIN__) || defined(_MSC_VER)

static int
_addImageSectionData(struct dl_phdr_info *info, size_t size, void *data) {
  InspectArgs *inspectArgs = (InspectArgs *)data;
  // inspectArgs contains addImage*Block function and the section name
#if defined(_MSC_VER)
  HMODULE handle;

  if (!info->dlpi_name || info->dlpi_name[0] == '\0')
    handle = GetModuleHandle(nullptr);
  else
    handle = GetModuleHandle(info->dlpi_name);
#else
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0')
    handle = dlopen(nullptr, RTLD_LAZY);
  else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
#endif

  unsigned long imageBlockSize;
  const uint8_t *imageBlock =
    _swift_getSectionDataPE(handle, inspectArgs->sectionName,
                           imageBlockSize);

  if (imageBlock)
    inspectArgs->fnAddImageBlock(imageBlock, imageBlockSize);

#if defined(_MSC_VER)
  FreeLibrary(handle);
#else
  dlclose(handle);
#endif
  return 0;
}

void
swift::_swift_initializeCallbacksForSectionData(InspectArgs *inspectArgs) {
  _swift_dl_iterate_phdr(_addImageSectionData, inspectArgs);
}
#else
# error No known mechanism to inspect dynamic libraries on this platform.
#endif
