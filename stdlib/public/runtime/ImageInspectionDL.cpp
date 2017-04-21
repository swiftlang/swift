//===--- ImageInspectionDL.cpp - Mach-O/ELF/Win32 shared libdl emulation --===//
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
// This file contains code used by ImageInspectionMachO.cpp,
// ImageInspectionELF.cpp and ImageInspectionWin32.cpp (CYGWIN). It is used
// via #include.
//
//===----------------------------------------------------------------------===//

#include "../SwiftShims/ImageInspectionShims.h"
#include <cstring>

int swift::lookupSymbol(const void *address, SymbolInfo *info) {
  Dl_info dlinfo;
  if (dladdr(address, &dlinfo) == 0) {
    return 0;
  }

  info->fileName = dlinfo.dli_fname;
  info->baseAddress = dlinfo.dli_fbase;
  info->symbolName = dlinfo.dli_sname;
  info->symbolAddress = dlinfo.dli_saddr;
  return 1;
}


static
void saveError(char **error) {
  if (error) {
    char *msg = dlerror();
    if (msg) {
      *error = strdup(msg);
    } else {
      *error = nullptr;
    }
  }
}

SWIFT_RUNTIME_STDLIB_INTERFACE
void *swift::_swift_stdlib_dlopen(const char *filename, int flags,
                                  char **error) {
  void *ret = dlopen(filename, flags);
  saveError(error);
  return ret;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_dlclose(void *handle, char **error) {
  int ret = dlclose(handle);
  saveError(error);
  return ret;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
void *swift::_swift_stdlib_dlsym(void *handle, const char *symbol,
                                   char **error) {
  void *ret = dlsym(handle, symbol);
  saveError(error);
  return ret;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_dladdr(void *addr, Dl_info *info) {
  return dladdr(addr, info);
}
