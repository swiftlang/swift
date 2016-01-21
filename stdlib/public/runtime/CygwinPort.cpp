//===--- CygwinPort.cpp - Functions for Cygwin port ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementations Cygwin specific functions needed for running Swift.
//
//===----------------------------------------------------------------------===//

#include "Private.h"
#include "swift/Runtime/Debug.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <windows.h>
#include <psapi.h>

using namespace swift;

int swift::_swift_dl_iterate_phdr(int (*Callback)(struct dl_phdr_info *info,
                                                  size_t size, void *data),
                                  void *data) {
  DWORD procId = GetCurrentProcessId();
  HANDLE procHandle =
      OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, procId);
  if (!procHandle) {
    swift::fatalError("OpenProcess() failed");
    return 0;
  }

  int lastRet = 0;

  // HMODULE modules[1024];
  std::vector<HMODULE> modules(1024);
  DWORD neededSize;

  BOOL ret = EnumProcessModules(procHandle, modules.data(),
                                modules.size() * sizeof(HMODULE), &neededSize);

  if (ret == FALSE) {
    swift::fatalError("EnumProcessModules() failed");
    return 0;
  }

  if (modules.size() * sizeof(HMODULE) < neededSize) {
    modules.resize(neededSize / sizeof(HMODULE));
    ret = EnumProcessModules(procHandle, modules.data(),
                             modules.size() * sizeof(HMODULE), &neededSize);
  }

  if (ret == FALSE) {
    swift::fatalError("EnumProcessModules() failed");
    return 0;
  }

  for (unsigned int i = 0; i < neededSize / sizeof(HMODULE); i++) {
    char modName[MAX_PATH];

    if (!GetModuleFileNameExA(procHandle, modules[i], modName,
                              sizeof(modName))) {
      swift::fatalError("GetModuleFileNameExA() failed");
    }

    dl_phdr_info hdr;
    hdr.dlpi_name = modName;
    hdr.dlpi_addr = modules[i];

    lastRet = Callback(&hdr, sizeof(hdr), data);
    if (lastRet != 0)
      break;
  }

  CloseHandle(procHandle);

  return lastRet;
}

uint8_t *swift::_swift_getSectionDataPE(void *handle, const char *sectionName,
                                        unsigned long *sectionSize) {
  unsigned char *peStart = (unsigned char *)handle;

  int ntHeadersOffset = peStart[0x3C];

  bool assert1 =
      peStart[ntHeadersOffset] == 'P' && peStart[ntHeadersOffset + 1] == 'E';
  if (assert1 == false) {
    swift::fatalError("_swift_getSectionDataPE()'s finding PE failed");
  }

  unsigned char *coff = peStart + ntHeadersOffset + 4;

  int16_t numberOfSections = *(int16_t *)(coff + 2);

  // SizeOfOptionalHeader
  int16_t sizeOfOptionalHeader = *(int16_t *)(coff + 16);

  const int kCoffFileHeaderSize = 20;
  unsigned char *sectionTableBase =
      coff + kCoffFileHeaderSize + sizeOfOptionalHeader;

  // Section Header Record
  const int kSectionRecordSize = 40;

  unsigned char *sectionHeader = sectionTableBase;
  for (int i = 0; i < numberOfSections; i++) {
    uint32_t virtualSize = *(uint32_t *)&sectionHeader[8];
    uint32_t virtualAddress = *(uint32_t *)&sectionHeader[12];

    char nameOfThisSection[9];
    memcpy(nameOfThisSection, sectionHeader, 8);
    nameOfThisSection[8] = '\0';

    if (strcmp(sectionName, nameOfThisSection) == 0) {
      *sectionSize = virtualSize;
      return (uint8_t *)handle + virtualAddress;
    }
    sectionHeader += kSectionRecordSize;
  }

  return nullptr;
}

#include <mutex>

namespace std {

static std::unique_lock<std::mutex> *lock_ = nullptr;
static std::mutex mutex_;

template class function<void()>;
function<void()> __once_functor;

mutex &__get_once_mutex() { return mutex_; }

void __set_once_functor_lock_ptr(unique_lock<mutex> *__ptr) { lock_ = __ptr; }

#if 1
extern "C" void __once_proxy() {
  function<void()> once_functor = std::move(__once_functor);
  unique_lock<mutex> *lock = lock_;
  if (lock != nullptr) {
    lock_ = nullptr;
    lock->unlock();
  }

  once_functor();
}
#endif
}
