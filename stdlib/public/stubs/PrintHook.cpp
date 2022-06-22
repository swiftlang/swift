//===--- PrintHook.cpp ----------------------------------------------------===//
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

#include "swift/shims/PrintHook.h"

#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"

using namespace swift;

struct PrintHook {
  void SWIFT_CC(swift) (* function)(const char *, bool, SWIFT_CONTEXT void *);
  void *context;
};
static Lazy<ConcurrentReadableArray<PrintHook>> printHookList;

bool _swift_isPrintHookInstalled(void) {
  return printHookList->snapshot().count() != 0;
}

void _swift_installPrintHook(
  void SWIFT_CC(swift) (* hook)(
    const char *message,
    bool isDebug,
    SWIFT_CONTEXT void *context
  ),
  void *hookContext
) {
  if (hookContext) {
    hookContext = swift_unknownObjectRetain(hookContext);
  }
  PrintHook newHook { hook, hookContext };
  printHookList->push_back(newHook);
}

void _swift_invokePrintHooks(const char *message, bool isDebug) {
  for (const PrintHook& hook : printHookList->snapshot()) {
    if (hook.function) {
      (* hook.function)(message, isDebug, hook.context);
    }
  }
}
