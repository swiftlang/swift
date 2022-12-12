//===--- SymbolInfo.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
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
#include <Psapi.h>
#include <shlwapi.h>
#elif SWIFT_STDLIB_HAS_DLADDR
#include <dlfcn.h>
#endif

#include <array>
#include <cstdlib>

#include "ImageInspection.h"
#include "swift/Runtime/Win32.h"

using namespace swift;

SymbolInfo::SymbolInfo()
#if defined(_WIN32) && !defined(__CYGWIN__)
: _package({}),
#elif SWIFT_STDLIB_HAS_DLADDR
: _info({}),
#endif
_address(nullptr)
{
}

SymbolInfo::~SymbolInfo() {
#if defined(_WIN32) && !defined(__CYGWIN__)
  if (char *imagePath = _imagePath.getValueOr(nullptr)) {
    free(imagePath);
  }
#endif
}

const void *SymbolInfo::getAddress() const {
  return _address;
}

const char *SymbolInfo::getImagePath() const {
#if defined(_WIN32) && !defined(__CYGWIN__)
  if (!_imagePath.has_value()) {
    // Call GetMappedFileNameW() instead of SymGetModuleInfoW() because it
    // doesn't require the use of the global DbgHelp lock and doesn't fetch
    // other information we don't need.
    std::array<wchar_t, MAX_PATH> filename;
    if (GetMappedFileNameW(GetCurrentProcess(), _address, filename.data(), filename.size())) {
      _imagePath = _swift_win32_copyUTF8FromWide(filename.data());
    }
  }

  return _imagePath.getValueOr(nullptr);
#elif SWIFT_STDLIB_HAS_DLADDR
  return _info.dli_fname;
#else
  return nullptr;
#endif
}

const char *SymbolInfo::getImageFilename() const {
  // TODO: if we adopt C++17, consider using std::filesystem::path::filename()
  const char *imagePath = getImagePath();
  if (!imagePath) {
    return nullptr;
  }
#if defined(_WIN32) && !defined(__CYGWIN__)
  return PathFindFileNameA(imagePath);
#else
  if (const char *lastSlash = strrchr(imagePath, '/')) {
    return lastSlash + 1;
  } else {
    return imagePath;
  }
#endif
}

const void *SymbolInfo::getImageBaseAddress() const {
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

    // If there are real-world examples of symbol names with Unicode characters
    // that we need to consider, SymbolInfo can hold a SYMBOL_INFO_PACKAGEW and
    // call SymFromAddrW(), then lazily convert to UTF-8 on the first call to
    // getSymbolName() as is done in getImagePath().
    SymbolInfo info;
    info._package.si.SizeOfStruct = sizeof(SYMBOL_INFO);
    info._package.si.MaxNameLen = MAX_SYM_NAME;
    if (SymFromAddr(hProcess, reinterpret_cast<const DWORD64>(address),
                    nullptr, &info._package.si)) {
      // Symbols without debug information available, such as those from the OS,
      // may not resolve a module base address. We can resolve it manually by
      // calling SymGetModuleBase() here.
      if (info._package.si.Address != 0 && info._package.si.ModBase == 0) {
        info._package.si.ModBase = SymGetModuleBase(hProcess,
                                                    info._package.si.Address);
      }
      result = info;
    }
  });
#elif SWIFT_STDLIB_HAS_DLADDR
  SymbolInfo info;
  if (dladdr(address, &info._info)) {
    result = info;
  }
#endif

  if (result.has_value()) {
    result->_address = address;
  }

  return result;
}

