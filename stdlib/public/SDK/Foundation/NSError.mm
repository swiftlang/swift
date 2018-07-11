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

#include "NSError.h"

#include "swift/Demangling/ManglingMacros.h"

using namespace swift;

// Declare the mangled Swift symbols that we'll be putting in the bridging info.
extern "C" const SWIFT_CC(swift) WitnessTable *
  MANGLE_SYM(So10CFErrorRefas5Error10FoundationWa)();

extern "C" const SWIFT_CC(swift) hashable_support::HashableWitnessTable *
  MANGLE_SYM(So8NSObjectCSH10ObjectiveCWa)();

extern "C" SWIFT_CC(swift)
  NSDictionary *MANGLE_SYM(10Foundation24_getErrorDefaultUserInfoyyXlSgxs0C0RzlF)(
    const OpaqueValue *error, const Metadata *T, const WitnessTable *Error);

extern "C" SWIFT_CC(swift) bool
  MANGLE_SYM(10Foundation21_bridgeNSErrorToError_3outSbSo0C0C_SpyxGtAA021_ObjectiveCBridgeableE0RzlF)(
    NSError *, OpaqueValue*, const Metadata *, const WitnessTable *);

extern "C" const ProtocolDescriptor
  MANGLE_SYM(10Foundation26_ObjectiveCBridgeableErrorMp);

// Define the bridging info struct.
extern "C" ErrorBridgingInfo ERROR_BRIDGING_SYMBOL_NAME = {
  MANGLE_SYM(So10CFErrorRefas5Error10FoundationWa),
  MANGLE_SYM(So8NSObjectCSH10ObjectiveCWa),
  MANGLE_SYM(10Foundation24_getErrorDefaultUserInfoyyXlSgxs0C0RzlF),
  MANGLE_SYM(10Foundation21_bridgeNSErrorToError_3outSbSo0C0C_SpyxGtAA021_ObjectiveCBridgeableE0RzlF),
  &MANGLE_SYM(10Foundation26_ObjectiveCBridgeableErrorMp)
};

// This directive ensures that the symbol is preserved even when statically
// linked into an executable and stripped, so that the dlsym lookup from
// ErrorObject.mm still works.
asm(".desc _" ERROR_BRIDGING_SYMBOL_NAME_STRING ", 0x10");
