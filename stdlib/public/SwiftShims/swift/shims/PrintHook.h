//===--- PrintHook.h ------------------------------------------------------===//
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

#ifndef SWIFT_STDLIB_SHIMS_PRINTHOOK_H
#define SWIFT_STDLIB_SHIMS_PRINTHOOK_H

#include "Visibility.h"
#include "SwiftStdbool.h"

#ifdef __cplusplus
extern "C" {
#endif

SWIFT_RUNTIME_STDLIB_SPI
__swift_bool _swift_isPrintHookInstalled(void);

SWIFT_RUNTIME_STDLIB_SPI
void _swift_installPrintHook(
  void __attribute__((swiftcall)) (* hook)(
    const char *message,
    __swift_bool isDebug,
    __attribute__((swift_context)) void *context
  ),
  void *hookContext
);

SWIFT_RUNTIME_STDLIB_SPI
void _swift_invokePrintHooks(const char *message, __swift_bool isDebug);

#ifdef __cplusplus
} // extern "C"
#endif

#endif
