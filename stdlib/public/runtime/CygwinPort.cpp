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

#include "swift/Runtime/Debug.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <psapi.h>

struct dl_phdr_info {
  void        *dlpi_addr;
  const char  *dlpi_name;
};

static void swift_systemCallFailure(const char *caller,
                                    const char *FunctionName) {
  swift::fatalError("Win32 system call '%s' failed at '%s'\n", FunctionName,
                    caller);
}

int swift_dl_iterate_phdr(int(*Callback)(struct dl_phdr_info *info, size_t size,
                                         void *data),
                          void *data) {
  DWORD ProcId = GetCurrentProcessId();
  HANDLE ProcHandle = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
                                   FALSE, ProcId);
  if (NULL == ProcHandle) {
    swift_systemCallFailure("swift_dl_iterate_phdr", "OpenProcess");
    return 0;		
  }

  int LastRet = 0;

  HMODULE modules[1024];
  DWORD NeededSize;

  BOOL ret = EnumProcessModules(ProcHandle, modules, sizeof(modules),
                                &NeededSize);

  if (ret == FALSE || sizeof(modules) < NeededSize) {
    swift_systemCallFailure("swift_dl_iterate_phdr", "EnumProcessModules");
    CloseHandle(ProcHandle);
    return  0;
  }

  for (unsigned int i = 0; i<NeededSize/sizeof(HMODULE); i++) {
    char ModName[MAX_PATH];

    if (GetModuleFileNameExA(ProcHandle, modules[i], ModName,
                             sizeof(ModName))) {
      dl_phdr_info  hdr;
      hdr.dlpi_name = ModName;
      hdr.dlpi_addr = modules[i];

      LastRet = Callback(&hdr, sizeof(hdr), data);
      if (LastRet != 0)
        break;
    } else {
      swift_systemCallFailure("swift_dl_iterate_phdr", "GetModuleFileNameExA");
    }
  }

  CloseHandle(ProcHandle);

  return  LastRet;
}

uint8_t *swift_getSectionDataPE(void *Handle, const char *SectionName,
                                unsigned long *section_size) {
  unsigned char *PEStart = (unsigned char *)Handle;

  int NtHeadersOffset = PEStart[0x3C];

  bool  assert1 = PEStart[NtHeadersOffset] == 'P' && 
                  PEStart[NtHeadersOffset + 1] == 'E';
  if (assert1 == false) {
    return  nullptr;
  }

  unsigned char *Coff = PEStart + NtHeadersOffset + 4;

  int16_t    NumberOfSections = *(int16_t *)(Coff + 2);

  // SizeOfOptionalHeader
  int16_t SizeOfOptionalHeader = *(int16_t *)(Coff + 16);

  const int kCoffFileHeaderSize = 20;
  unsigned char *SectionTableBase = Coff + kCoffFileHeaderSize + 
                                      SizeOfOptionalHeader;

  // Section Header Record
  const int kSectionRecordSize = 40;

  unsigned char *SectionHeader = SectionTableBase;
  for (int i = 0; i < NumberOfSections; i++) {
    uint32_t  VirtualSize = *(uint32_t *)&SectionHeader[8];
    uint32_t  VirtualAddress = *(uint32_t *)&SectionHeader[12];

    char NameOfThisSection[9];
    memcpy(NameOfThisSection, SectionHeader, 8);
    NameOfThisSection[8] = '\0';

    if (strcmp(SectionName, NameOfThisSection) == 0) {
      *section_size = VirtualSize;
      return  (uint8_t *)Handle + VirtualAddress;
    }
    SectionHeader += kSectionRecordSize;
  }

  return  nullptr;
}
