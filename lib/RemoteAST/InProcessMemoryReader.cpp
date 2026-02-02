//===--- InProcessMemoryReader.cpp - Reads local memory -------------------===//
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
//  This file implements the abstract interface for working with remote memory.
//  This method cannot be implemented in the header, as we must avoid importing
//  <windows.h> in a header, which causes conflicts with Swift definitions.
//
//===----------------------------------------------------------------------===//

#include "swift/Remote/InProcessMemoryReader.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

using namespace swift;
using namespace swift::remote;

RemoteAddress InProcessMemoryReader::getSymbolAddress(const std::string &name) {
#if defined(_WIN32)
    auto pointer = GetProcAddress(GetModuleHandle(NULL), name.c_str());
#else
    auto pointer = dlsym(RTLD_DEFAULT, name.c_str());
#endif
    return RemoteAddress((uint64_t)pointer, RemoteAddress::DefaultAddressSpace);
}
