//===--- ImageInspectionWin32.cpp - Win32 image inspection ----------------===//
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

#include "SymbolInfo.h"

#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#include <DbgHelp.h>
#elif SWIFT_STDLIB_HAS_DLADDR
#include <dlfcn.h>
#endif

#include "ImageInspection.h"

using namespace swift;

const char *SymbolInfo::getFilename() const {
#if defined(_WIN32) && !defined(__CYGWIN__)
  return nullptr;
#elif SWIFT_STDLIB_HAS_DLADDR
  return _info.dli_fname;
#else
  return nullptr;
#endif
}

const void *SymbolInfo::getBaseAddress() const {
#if defined(_WIN32) && !defined(__CYGWIN__)
  return reinterpret_cast<const void *>(_package.si.ModBase);
#elif SWIFT_STDLIB_HAS_DLADDR
  return _info.dli_fbase;
#else
  return nullptr;
#endif
}

const char *SymbolInfo::getSymbolName() const {
#if defined(_WIN32) && !defined(__CYGWIN__)
  return _package.si.Name;
#elif SWIFT_STDLIB_HAS_DLADDR
  return _info.dli_sname;
#else
  return nullptr;
#endif
}

const void *SymbolInfo::getSymbolAddress() const {
#if defined(_WIN32) && !defined(__CYGWIN__)
  return reinterpret_cast<const void *>(_package.si.Address);
#elif SWIFT_STDLIB_HAS_DLADDR
  return _info.dli_saddr;
#else
  return nullptr;
#endif
}

llvm::Optional<SymbolInfo> SymbolInfo::lookup(const void *address) {
  llvm::Optional<SymbolInfo> result;

#if defined(__wasm__)
  // Currently, Wasm doesn't have a standard stable ABI for exporting address <->
  // symbol table, it's work in progress. Also, there is no API to access such
  // information from Wasm binary side. It's accessible only from host VM.
  // See https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md
#elif defined(_WIN32) && !defined(__CYGWIN__)
  _swift_win32_withDbgHelpLibrary([&] (HANDLE hProcess) {
    if (!hProcess) {
        return;
    }

    SymbolInfo info;
    info._package.si.SizeOfStruct = sizeof(SYMBOL_INFO);
    info._package.si.MaxNameLen = MAX_SYM_NAME;
    if (SymFromAddr(hProcess, reinterpret_cast<const DWORD64>(address),
                    nullptr, &info._package.si)) {
      result = info;
    }
  });
#elif SWIFT_STDLIB_HAS_DLADDR
  SymbolInfo info;
  if (dladdr(address, &info._info)) {
    result = info;
  }
#endif

  return result;
}
