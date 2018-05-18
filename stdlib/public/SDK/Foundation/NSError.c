//===----------------------------------------------------------------------===//
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

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#include "swift/Demangling/ManglingMacros.h"

// Declare dynamic lookup points for ErrorObject.mm. It uses dlsym to
// find these symbols to locate NS/CFError Error conformance tables and
// related items. The .desc asm directive ensures that they are
// preserved even when statically linked into an executable and
// stripped, so that the dlsym lookup still works.
#define ERROROBJECT_DYNAMIC_LOOKUP_POINT(symbol) \
  extern void *MANGLE_SYM(symbol); \
  void **MANGLING_CONCAT2(ErrorObjectLookup_, MANGLE_SYM(symbol)) = &MANGLE_SYM(symbol); \
  asm(".desc _ErrorObjectLookup_" MANGLE_AS_STRING(MANGLE_SYM(symbol)) ", 0x10");

ERROROBJECT_DYNAMIC_LOOKUP_POINT(So10CFErrorRefas5Error10FoundationWa)
ERROROBJECT_DYNAMIC_LOOKUP_POINT(So8NSObjectCs8Hashable10ObjectiveCWa)
ERROROBJECT_DYNAMIC_LOOKUP_POINT(10Foundation24_getErrorDefaultUserInfoyyXlSgxs0C0RzlF)
ERROROBJECT_DYNAMIC_LOOKUP_POINT(10Foundation21_bridgeNSErrorToError_3outSbSo0C0C_SpyxGtAA021_ObjectiveCBridgeableE0RzlF)
ERROROBJECT_DYNAMIC_LOOKUP_POINT(10Foundation26_ObjectiveCBridgeableErrorMp)

#endif
