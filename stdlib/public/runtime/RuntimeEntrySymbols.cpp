//===--- RuntimeEntrySymbols.cpp - Define symbols for runtime entries -----===//
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
//  This file defines function pointer symbols for runtime entries.
//
//===----------------------------------------------------------------------===//

// Produce global symbols referencing runtime function implementations only for
// those runtime entries that demand it.
//
// Each runtime function definition in RuntimeFunctions.def should
// indicate if it requires a global referencing it.
//
// For example, all entries using the RegisterPreservingCC calling convention
// need a global, because they are currently always invoked indirectly using a
// client-side wrapper.
//
// Runtime entries using the DefaultCC calling convention do not
// demand a global symbol by default. But they can optionally ask for it, in
// case it is needed. For example, _swift_isDeallocating is required by
// Instruments.

#include "swift/Runtime/Config.h"

// Entry points using a standard C calling convention or not using the new
// calling convention do not need to have global symbols referring to their
// implementations.
#define FOR_CONV_DefaultCC(...)
#define FOR_CONV_C_CC(...)
// Entry points using the new calling convention require global symbols
// referring to their implementations.
#define FOR_CONV_RegisterPreservingCC(x) x

typedef void (*RuntimeEntry)();

// Generate a forward declaration of the runtime entry implementation.
// Define a global symbol referring to this implementation.

#define DEFINE_SYMBOL(SymbolName, Name, CC)                                    \
  SWIFT_RT_ENTRY_IMPL_VISIBILITY extern "C" void Name()                        \
      SWIFT_CC(CC);                                                            \
  SWIFT_RUNTIME_EXPORT RuntimeEntry SymbolName =                    \
      reinterpret_cast<RuntimeEntry>(Name);

#define FUNCTION1(Id, Name, CC, ReturnTys, ArgTys, Attrs)                      \
  DEFINE_SYMBOL(SWIFT_RT_ENTRY_REF(Name), Name, CC)

#if defined(SWIFT_RT_USE_WRAPPERS)
// Automatically generate a global symbol name if it is required by the calling
// convention.
#define FUNCTION(Id, Name, CC, ReturnTys, ArgTys, Attrs)                       \
  FOR_CONV_##CC(FUNCTION1(Id, Name, CC, ReturnTys, ArgTys, Attrs))
#else
// No need to generate any global symbols for entries that do not provide their
// own symbols.
#define FUNCTION(Id, Name, CC, ReturnTys, ArgTys, Attrs)
#endif
// Allow for a custom global symbol name and implementation.
#define FUNCTION_WITH_GLOBAL_SYMBOL_AND_IMPL(Id, Name, GlobalSymbolName, Impl, \
                                             CC, ReturnTys, ArgTys, Attrs)     \
  DEFINE_SYMBOL(GlobalSymbolName, Impl, CC)

// Indicate that we are going to generate the global symbols for those runtime
// functions that require it.
#define SWIFT_RUNTIME_GENERATE_GLOBAL_SYMBOLS 1

namespace swift {
// Generate global symbols which are function pointers to the actual
// implementations of runtime entry points.
// This is done only for entry points using a new calling convention or
// for those entry points which explicitly require it.
#include "swift/Runtime/RuntimeFunctions.def"
} // namespace swift

