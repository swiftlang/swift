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
}

/// Instrument the runtime functions only if it is a build with
/// enabled assertions.
#if !defined(NDEBUG)

/// The name of a helper function for tracking the calls of a runtime function.
#define SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION, OBJ)                       \
  swift_trackRuntimeInvocation_##RT_FUNCTION

/// The name of the counter for tracking the calls of a runtime function.
#define SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION_NAME)            \
  invocationCounter_##RT_FUNCTION_NAME

/// Invoke a helper function for tracking the calls of a runtime function.
#define SWIFT_RT_TRACK_INVOCATION(OBJ, RT_FUNCTION)                            \
  SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION, OBJ)(OBJ)

/// Set of runtime functions that whose invocations need to be tracked.
/// Edit this list to add new runtime functions or remove the old ones.
#define RT_FUNCTIONS_TO_TRACK                                                  \
  RT_FUNCTION_TO_TRACK(object, swift_retain)                                   \
  RT_FUNCTION_TO_TRACK(object, swift_release)                                  \
  RT_FUNCTION_TO_TRACK(object, swift_retain_n)                                 \
  RT_FUNCTION_TO_TRACK(object, swift_release_n)                                \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_retain)                         \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_release)                        \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_retain_n)                       \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_release_n)                      \
  RT_FUNCTION_TO_TRACK(object, swift_setDeallocating)                          \
  RT_FUNCTION_TO_TRACK(object, swift_unownedRetain)                            \
  RT_FUNCTION_TO_TRACK(object, swift_unownedRelease)                           \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unownedRetain)                  \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unownedRelease)                 \
  RT_FUNCTION_TO_TRACK(object, swift_unownedRetain_n)                          \
  RT_FUNCTION_TO_TRACK(object, swift_unownedRelease_n)                         \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unownedRetain_n)                \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unownedRelease_n)               \
  RT_FUNCTION_TO_TRACK(object, swift_tryPin)                                   \
  RT_FUNCTION_TO_TRACK(object, swift_unpin)                                    \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_tryPin)                         \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unpin)                          \
  RT_FUNCTION_TO_TRACK(object, swift_tryRetain)                                \
  RT_FUNCTION_TO_TRACK(object, swift_tryRelease)                               \
  RT_FUNCTION_TO_TRACK(object, swift_unownedRetainStrong)                      \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unownedRetainStrong)            \
  RT_FUNCTION_TO_TRACK(object, swift_unownedRetainStrongAndRelease)            \
  RT_FUNCTION_TO_TRACK(object, swift_nonatomic_unownedRetainStrongAndRelease)

#undef RT_FUNCTION_TO_TRACK
#define RT_FUNCTION_TO_TRACK(OBJ, RT_FUNCTION)                                 \
  extern void SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION,                      \
                                             OBJ)(swift::HeapObject * OBJ);

/// Declarations of external functions for invocations tracking
RT_FUNCTIONS_TO_TRACK

#undef RT_FUNCTION_TO_TRACK
#define RT_FUNCTION_TO_TRACK(OBJ, RT_FUNCTION_NAME)                            \
  uint32_t SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION_NAME) = 0;

#else

/// It is just a NOP if assertions are not enabled.
#define SWIFT_RT_TRACK_INVOCATION(OBJ, RT_FUNCTION)

#define RT_FUNCTIONS_TO_TRACK

#endif // NDEBUG

/// This type defines a callback to be called on any intercepted runtime
/// function.
using RuntimeFunctionCountersUpdateHandler =
  __attribute__((swiftcall))
  void (*)(swift::HeapObject *object, int64_t runtimeFunctionID);

// Define counters used for tracking the total number of invocations of runtime
// functions.
struct RuntimeFunctionCountersState {
  // Provide one counter per runtime function being tracked.
  RT_FUNCTIONS_TO_TRACK
};

/// Public APIs

/// Get the runtime object state associated with an object and store it
/// into the result.
extern "C" void
getObjectRuntimeFunctionCounters(swift::HeapObject *object,
                                 RuntimeFunctionCountersState &result);

/// Get the global runtime state containing the total numbers of invocations for
/// each runtime function of interest and store it into the result.
extern "C" void
getGlobalRuntimeFunctionCounters(RuntimeFunctionCountersState &result);

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
setGlobalRuntimeFunctionCounters(RuntimeFunctionCountersState &state);

/// Set the runtime object state associated with an object from a provided
/// state.
extern "C" void
setObjectRuntimeFunctionCounters(swift::HeapObject *object,
                                 RuntimeFunctionCountersState &state);

/// Set the global runtime function counters update handler.
extern "C" RuntimeFunctionCountersUpdateHandler
setGlobalRuntimeFunctionCountersUpdateHandler(
    RuntimeFunctionCountersUpdateHandler handler);

#endif
