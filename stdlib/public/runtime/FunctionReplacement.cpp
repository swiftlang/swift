//===--- FunctionReplacement.cpp ------------------------------------------===//
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

#include "swift/Runtime/FunctionReplacement.h"
#include "SwiftTLSContext.h"

using namespace swift;
using namespace swift::runtime;

char *swift::swift_getFunctionReplacement(char **ReplFnPtr, char *CurrFn) {
  char *ReplFn = *ReplFnPtr;
  char *RawReplFn = ReplFn;

#if SWIFT_PTRAUTH
  RawReplFn = ptrauth_strip(RawReplFn, ptrauth_key_function_pointer);
#endif
  if (RawReplFn == CurrFn)
    return nullptr;

  auto &ctx = SwiftTLSContext::get();
  if (ctx.CallOriginalOfReplacedFunction) {
    ctx.CallOriginalOfReplacedFunction = false;
    return nullptr;
  }
  return ReplFn;
}

char *swift::swift_getOrigOfReplaceable(char **OrigFnPtr) {
  char *OrigFn = *OrigFnPtr;
  SwiftTLSContext::get().CallOriginalOfReplacedFunction = true;
  return OrigFn;
}
