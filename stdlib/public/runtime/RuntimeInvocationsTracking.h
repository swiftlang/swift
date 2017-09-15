//===--- RuntimeInvocationsTracking.h ---------------------------*- C++ -*-===//
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
// Track invocations of Swift runtime functions. This can be used for
// performance analysis.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_RUNTIME_INVOCATIONS_TRACKING_H
#define SWIFT_STDLIB_RUNTIME_INVOCATIONS_TRACKING_H

namespace swift {
struct HeapObject;
struct RuntimeFunctionCountersState;
} // end namespace swift

/// Instrument the runtime functions only if we are building with
/// assertions enabled.
#if !defined(NDEBUG)

/// The name of a helper function for tracking the calls of a runtime function.
#define SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION)                            \
  swift_trackRuntimeInvocation_##RT_FUNCTION

/// Invoke a helper function for tracking the calls of a runtime function.
#define SWIFT_RT_TRACK_INVOCATION(OBJ, RT_FUNCTION)                            \
  SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION)(OBJ)

#define FUNCTION_TO_TRACK(RT_FUNCTION)                                      \
  extern void SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION)(swift::HeapObject *  \
                                                          OBJ);

/// Declarations of external functions for invocations tracking.
#include "RuntimeInvocationsTracking.def"

#else

/// It is just a NOP if assertions are not enabled.
#define SWIFT_RT_TRACK_INVOCATION(OBJ, RT_FUNCTION)

#endif // NDEBUG

/// This type defines a callback to be called on any intercepted runtime
/// function.
using RuntimeFunctionCountersUpdateHandler =
  __attribute__((swiftcall))
  void (*)(swift::HeapObject *object, int64_t runtimeFunctionID);

/// Public APIs

/// Get the runtime object state associated with an object and store it
/// into the result.
extern "C" void
getObjectRuntimeFunctionCounters(swift::HeapObject *object,
                                 swift::RuntimeFunctionCountersState *result);

/// Get the global runtime state containing the total numbers of invocations for
/// each runtime function of interest and store it into the result.
extern "C" void
getGlobalRuntimeFunctionCounters(swift::RuntimeFunctionCountersState *result);

/// Return the names of the runtime functions being tracked.
/// Their order is the same as the order of the counters in the
/// RuntimeObjectState structure.
extern "C" const char **getRuntimeFunctionNames();

/// Return the offsets of the runtime function counters being tracked.
/// Their order is the same as the order of the counters in the
/// RuntimeFunctionCountersState structure.
extern "C" const uint16_t *getRuntimeFunctionCountersOffsets();

/// Return the number of runtime functions being tracked.
extern "C" uint64_t getNumRuntimeFunctionCounters();

/// Dump all per-object runtime function pointers.
extern "C" void dumpObjectsRuntimeFunctionPointers();

/// Set mode for global runtime function counters.
/// Return the old value of this flag.
extern "C" int setPerObjectRuntimeFunctionCountersMode(int mode);

/// Set mode for per object runtime function counters.
/// Return the old value of this flag.
extern "C" int setGlobalRuntimeFunctionCountersMode(int mode);

/// Set the global runtime state of function pointers from a provided state.
extern "C" void
setGlobalRuntimeFunctionCounters(swift::RuntimeFunctionCountersState *state);

/// Set the runtime object state associated with an object from a provided
/// state.
extern "C" void
setObjectRuntimeFunctionCounters(swift::HeapObject *object,
                                 swift::RuntimeFunctionCountersState *state);

/// Set the global runtime function counters update handler.
extern "C" RuntimeFunctionCountersUpdateHandler
setGlobalRuntimeFunctionCountersUpdateHandler(
    RuntimeFunctionCountersUpdateHandler handler);

#endif
