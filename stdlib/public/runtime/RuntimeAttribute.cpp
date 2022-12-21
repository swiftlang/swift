//===---- AccessibleFunction.cpp - Swift protocol conformance checking ----===//
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
//
// Checking and caching of Swift runtime attributes.
//
//===----------------------------------------------------------------------===//

#include "ImageInspection.h"
#include "swift/Basic/Lazy.h"

using namespace swift;

// TODO: implement me!
static Lazy<int> Functions;

void
swift::addImageRuntimeAttributesBlockCallbackUnsafe(const void *baseAddress,
                                                    const void *attribs,
                                                    uintptr_t size) {
    auto &C = Functions.unsafeGetAlreadyInitialized();
    // TODO: implement me!
    // _registerRuntimeAttributes(C, RuntimeAttributesSection{attribs, size});
}

void
swift::addImageRuntimeAttributesBlockCallback(const void *baseAddress,
                                              const void *attribs,
                                              uintptr_t size) {
    Functions.get();
    addImageRuntimeAttributesBlockCallbackUnsafe(baseAddress, attribs, size);
}
