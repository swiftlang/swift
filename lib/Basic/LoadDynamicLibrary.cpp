//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LoadDynamicLibrary.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/Windows/WindowsSupport.h"
#include "swift/Basic/LLVM.h"
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#if defined(_WIN32)
void *swift::loadLibrary(const char *path, std::string *err) {
  SmallVector<wchar_t, MAX_PATH> pathUnicode;
  if (std::error_code ec = llvm::sys::windows::UTF8ToUTF16(path, pathUnicode)) {
    SetLastError(ec.value());
    llvm::MakeErrMsg(err, std::string(path) + ": Can't convert to UTF-16");
    return nullptr;
  }

  HMODULE handle = LoadLibraryW(pathUnicode.data());
  if (handle == NULL) {
    llvm::MakeErrMsg(err, std::string(path) + ": Can't open");
    return nullptr;
  }
  return (void *)handle;
}

void *swift::getAddressOfSymbol(void *handle, const char *symbol) {
  return (void *)uintptr_t(GetProcAddress((HMODULE)handle, symbol));
}

#else
void *swift::loadLibrary(const char *path, std::string *err) {
  void *handle = ::dlopen(path, RTLD_LAZY | RTLD_LOCAL);
  if (!handle)
    *err = ::dlerror();
  return handle;
}

void *swift::getAddressOfSymbol(void *handle, const char *symbol) {
  return ::dlsym(handle, symbol);
}
#endif