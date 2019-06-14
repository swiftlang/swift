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

#include "Overrides.h"
#include "swift/Runtime/Once.h"
#include "swift/Runtime/Exclusivity.h"
#include "../runtime/ThreadLocalStorage.h"

using namespace swift;

// The thread-local key must be weak so that it is shared between different
// binaries (e.g. shared libraries).
__attribute__((weak)) __swift_thread_key_t _swift_dr_key;
__attribute__((weak)) swift_once_t _swift_dr_Predicate;

static void createSwiftThreadKey(void *) {
  int result = SWIFT_THREAD_KEY_CREATE(&_swift_dr_key, nullptr);
  if (result != 0)
    abort();
}

static __swift_thread_key_t &getTLSKey() {
  swift_once(&_swift_dr_Predicate, createSwiftThreadKey, nullptr);
  return _swift_dr_key;
}

__attribute__((visibility("hidden"), weak))
extern "C" char *swift_getFunctionReplacement50(char **ReplFnPtr, char *CurrFn) {
  // Call the current implementation if it is available.
  if (swift_getFunctionReplacement)
    return swift_getFunctionReplacement(ReplFnPtr, CurrFn);

  char *ReplFn = *ReplFnPtr;
  char *RawReplFn = ReplFn;

  if (RawReplFn == CurrFn)
    return nullptr;

  __swift_thread_key_t key = getTLSKey();
  if ((intptr_t)SWIFT_THREAD_GETSPECIFIC(key) != 0) {
    SWIFT_THREAD_SETSPECIFIC(key, (void *)0);
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
  SWIFT_THREAD_SETSPECIFIC(getTLSKey(), (void *)-1);
  return OrigFn;
}
