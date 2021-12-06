//===---- AccessibleFunction.cpp - Swift protocol conformance checking ----===//
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
//
// Checking and caching of Swift accessible functions.
//
//===----------------------------------------------------------------------===//

#include "ImageInspection.h"
#include "Private.h"
#include "swift/Basic/Lazy.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "Private.h"

#include <cstdint>

using namespace swift;

#pragma mark Accessible function cache
namespace {

struct AccessibleFunctionsSection {
  const AccessibleFunctionRecord *Begin, *End;

  AccessibleFunctionsSection(const AccessibleFunctionRecord *begin,
                             const AccessibleFunctionRecord *end)
      : Begin(begin), End(end) {}

  AccessibleFunctionsSection(const void *ptr, uintptr_t size) {
    auto bytes = reinterpret_cast<const char *>(ptr);
    Begin = reinterpret_cast<const AccessibleFunctionRecord *>(ptr);
    End = reinterpret_cast<const AccessibleFunctionRecord *>(bytes + size);
  }

  const AccessibleFunctionRecord *begin() const { return Begin; }
  const AccessibleFunctionRecord *end() const { return End; }
};

struct AccessibleFunctionsState {
  ConcurrentReadableArray<AccessibleFunctionsSection> SectionsToScan;

  AccessibleFunctionsState() {
    initializeAccessibleFunctionsLookup();
  }
};

static Lazy<AccessibleFunctionsState> Functions;

} // end anonymous namespace

static void _registerAccessibleFunctions(AccessibleFunctionsState &C,
                                         AccessibleFunctionsSection section) {
  C.SectionsToScan.push_back(section);
}

void swift::addImageAccessibleFunctionsBlockCallbackUnsafe(const void *functions,
                                                           uintptr_t size) {
  assert(
      size % sizeof(AccessibleFunctionRecord) == 0 &&
      "accessible function section not a multiple of AccessibleFunctionRecord");

  auto &C = Functions.unsafeGetAlreadyInitialized();
  _registerAccessibleFunctions(C, AccessibleFunctionsSection{functions, size});
}

void swift::addImageAccessibleFunctionsBlockCallback(const void *functions,
                                                     uintptr_t size) {
  Functions.get();
  addImageAccessibleFunctionsBlockCallbackUnsafe(functions, size);
}
