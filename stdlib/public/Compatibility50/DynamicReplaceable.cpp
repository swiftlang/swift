//===--- ProtocolConformance.cpp - Swift protocol conformance checking ----===//
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
// Runtime support for dynamic replaceable functions.
//
// This implementation is intended to be backward-deployed into Swift 5.0
// runtimes.
//
//===----------------------------------------------------------------------===//

#include "Overrides.h"
#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/Metadata.h"
#include "../runtime/ThreadLocalStorage.h"

using namespace swift;

// Mirror the SwiftTLSContext, which is defined in the runtime.
// This is a hack: the layout must match with SwiftTLSContext defined in
// Exclusivity.cpp. Fortunately it contains only a single pointer (the head of
// the access list).
class SwiftTLSContext {
  void *AccessHead;

public:
  void *getHead() const { return AccessHead; }

  void setExtraBit() {
    AccessHead = (void *)((intptr_t)AccessHead | 1);
  }

  void clearExtraBit() {
    AccessHead = (void *)((intptr_t)AccessHead & ~1);
  }

  bool testExtraBit() const {
    return (bool)((intptr_t)AccessHead & 1);
  }
};

__attribute__((visibility("hidden"), weak))
extern "C" char *swift_getFunctionReplacement(char **ReplFnPtr, char *CurrFn) {
  char *ReplFn = *ReplFnPtr;
  char *RawReplFn = ReplFn;

  if (RawReplFn == CurrFn)
    return nullptr;

  void *tlsStorage = SWIFT_THREAD_GETSPECIFIC(SWIFT_RUNTIME_TLS_KEY);
  if (tlsStorage) {
    SwiftTLSContext *ctx = static_cast<SwiftTLSContext*>(tlsStorage);

    if (ctx->testExtraBit()) {
      ctx->clearExtraBit();
      return nullptr;
    }
  }
  return ReplFn;
}

__attribute__((visibility("hidden"), weak))
extern "C" char *swift_getOrigOfReplaceable(char **OrigFnPtr) {
  void *tlsStorage = SWIFT_THREAD_GETSPECIFIC(SWIFT_RUNTIME_TLS_KEY);
  if (!tlsStorage) {
    ValueBuffer dummyAccess;
    int dummyValue = 0;
    swift_beginAccess(&dummyValue, &dummyAccess, ExclusivityFlags::Read, nullptr);
    swift_endAccess(&dummyAccess);
    tlsStorage = SWIFT_THREAD_GETSPECIFIC(SWIFT_RUNTIME_TLS_KEY);
    if (!tlsStorage)
      abort();
  }
  SwiftTLSContext *ctx = static_cast<SwiftTLSContext*>(tlsStorage);
  void *pr = ctx->getHead();
  ctx->setExtraBit();
  char *OrigFn = *OrigFnPtr;
  return OrigFn;
}

