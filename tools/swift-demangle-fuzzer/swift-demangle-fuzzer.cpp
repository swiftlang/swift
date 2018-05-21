//===--- swift-demangle-fuzzer.cpp - Swift fuzzer -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This program tries to fuzz the demangler shipped as part of the swift
// compiler.
// For this to work you need to pass --enable-sanitizer-coverage to build-script
// otherwise the fuzzer doesn't have coverage information to make progress
// (making the whole fuzzing operation really ineffective).
// It is recommended to use the tool together with another sanitizer to expose
// more bugs (asan, lsan, etc...)
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/ManglingMacros.h"
#include <stddef.h>
#include <stdint.h>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
  swift::Demangle::Context DCtx;
  swift::Demangle::DemangleOptions Opts;
  std::string NullTermStr((const char *)Data, Size);
  swift::Demangle::NodePointer pointer = DCtx.demangleSymbolAsNode(NullTermStr);
  return 0; // Non-zero return values are reserved for future use.
}
