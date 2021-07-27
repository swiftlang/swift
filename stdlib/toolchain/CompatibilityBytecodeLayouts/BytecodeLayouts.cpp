//===--- RuntimeValueWitness.cpp - Swift Language Value Witness Runtime
// Implementation---===//
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
// Implementations of runtime determined value witness functions
// This file is intended to be statically linked into executables until it is
// fully added to the runtime.
//
//===----------------------------------------------------------------------===//

#include "BytecodeLayouts.h"

using namespace swift;

__attribute__((weak)) extern "C" void
swift_generic_destroy(void *address, uint8_t *layout, uint32_t layoutLen) {
  assert(false && "nyi");
}

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden"))) extern "C" char
    _swift_FORCE_LOAD_$_swiftCompatibilityBytecodeLayouts = 0;
