//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Implementation of ImageInspection for WebAssembly.
///
//===----------------------------------------------------------------------===//

#if defined(__wasm__)

#include "../SwiftShims/MetadataSections.h"
#include "ImageInspection.h"

using namespace swift;

int swift::lookupSymbol(const void *address, SymbolInfo *info) {
  // Currently, Wasm doesn't have a standard stable ABI for exporting address <->
  // symbol table, it's work in progress. Also, there is no API to access such
  // information from Wasm binary side. It's accessible only from host VM. 
  // See https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md
  // Seems reasonable to use a stub for now.
  return 0;
}

#endif // defined(__wasm__)
