//===--- ProtocolConformance.cpp - Swift protocol conformance checking ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Runtime support for dynamic replaceable functions.
//
// This implementation is intended to be backward-deployed into Swift 5.0
// runtimes.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/FunctionReplacement.h"
#include "swift/Threading/ThreadLocalStorage.h"

using namespace swift;

namespace {

// Some threading libraries will need a global constructor here; that is
// unavoidable in the general case, so turn off the warning just for this line.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wglobal-constructors"
SWIFT_THREAD_LOCAL_TYPE(uintptr_t, tls_key::compatibility50) compat50Key;
#pragma clang diagnostic pop

} // namespace

__attribute__((visibility("hidden"), weak))
extern "C" char *swift_getFunctionReplacement50(char **ReplFnPtr, char *CurrFn) {
  // Call the current implementation if it is available.
  if (swift_getFunctionReplacement)
    return swift_getFunctionReplacement(ReplFnPtr, CurrFn);

  char *ReplFn = *ReplFnPtr;
  char *RawReplFn = ReplFn;

#if SWIFT_PTRAUTH
  RawReplFn = ptrauth_strip(RawReplFn, ptrauth_key_function_pointer);
#endif

  if (RawReplFn == CurrFn)
    return nullptr;

  auto origKey = compat50Key.get();
  if ((origKey & 0x1) != 0) {
    auto mask = ((uintptr_t)-1) << 1;
    auto resetKey = origKey & mask;
    compat50Key.set(resetKey);
    return nullptr;
  }
  return ReplFn;
}

__attribute__((visibility("hidden"), weak))
extern "C" char *swift_getOrigOfReplaceable50(char **OrigFnPtr) {
  // Call the current implementation if it is available.
  if (swift_getOrigOfReplaceable)
    return swift_getOrigOfReplaceable(OrigFnPtr);

  char *OrigFn = *OrigFnPtr;
  auto origKey = compat50Key.get();
  auto newKey = origKey | 0x1;
  compat50Key.set(newKey);
  return OrigFn;
}

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden")))
extern "C"
char _swift_FORCE_LOAD_$_swiftCompatibilityDynamicReplacements = 0;
