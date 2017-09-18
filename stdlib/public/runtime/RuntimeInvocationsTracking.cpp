//===--- RuntimeInvocationsTracking.cpp - Track runtime invocations -------===//
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
// Track invocations of Swift runtime functions. This can be used for performance
// analysis.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DenseMap.h"
#include "RuntimeInvocationsTracking.h"
#include "swift/Runtime/HeapObject.h"

#define SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION)                 \
  invocationCounter_##RT_FUNCTION

namespace swift {

// Define counters used for tracking the total number of invocations of runtime
// functions.
struct RuntimeFunctionCountersState {
#define FUNCTION_TO_TRACK(RT_FUNCTION)                                         \
  uint32_t SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION) = 0;
// Provide one counter per runtime function being tracked.
#include "RuntimeInvocationsTracking.def"
};

} // end namespace swift


/// If set, global runtime function counters should be tracked.
static bool UpdatePerObjectRuntimeFunctionCounters = false;
/// If set, per object runtime function counters should be tracked.
static bool UpdateGlobalRuntimeFunctionCounters = false;
/// TODO: Add support for enabling/disabling counters on a per object basis?

/// Global set of counters tracking the total number of runtime invocations.
static RuntimeFunctionCountersState RuntimeGlobalFunctionCountersState;

/// The object state cache mapping objects to the collected state associated with
/// them.
/// TODO: Do we need to make it thread-safe?
static llvm::DenseMap<HeapObject *, RuntimeFunctionCountersState> RuntimeObjectStateCache;

static const char *RuntimeFunctionNames[] {
/// Define names of runtime functions.
#define FUNCTION_TO_TRACK(RT_FUNCTION) #RT_FUNCTION,
#include "RuntimeInvocationsTracking.def"
  nullptr
};

#define RT_FUNCTION_ID(RT_FUNCTION) ID_##RT_FUNCTION

/// Define an enum where each enumerator corresponds to a runtime function being
/// tracked. Their order is the same as the order of the counters in the
/// RuntimeObjectState structure.
enum RuntimeFunctionNamesIDs : uint32_t {
/// Defines names of enum cases for each function being tracked.
#define FUNCTION_TO_TRACK(RT_FUNCTION) RT_FUNCTION_ID(RT_FUNCTION),
#include "RuntimeInvocationsTracking.def"
  ID_LastRuntimeFunctionName,
};

/// The global handler to be invoked on runtime function counters updates.
static RuntimeFunctionCountersUpdateHandler
    GlobalRuntimeFunctionCountersUpdateHandler;

/// The offsets of the runtime function counters being tracked inside the
/// RuntimeObjectState structure. The array is indexed by
/// the enumerators from RuntimeFunctionNamesIDs.
static uint16_t RuntimeFunctionCountersOffsets[] = {
/// Define offset for each function being tracked.
#define FUNCTION_TO_TRACK(RT_FUNCTION)                                         \
  (sizeof(uint16_t) * (unsigned)RT_FUNCTION_ID(RT_FUNCTION)),
#include "RuntimeInvocationsTracking.def"
};

/// Define implementations of tracking functions.
/// TODO: Track only objects that were registered for tracking?
/// TODO: Perform atomic increments?
#define FUNCTION_TO_TRACK(RT_FUNCTION)                                         \
  void SWIFT_RT_TRACK_INVOCATION_NAME(RT_FUNCTION)(HeapObject * object) {      \
    /* Update global counters. */                                              \
    if (UpdateGlobalRuntimeFunctionCounters) {                                 \
      RuntimeGlobalFunctionCountersState                                       \
          .SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION)++;           \
      if (GlobalRuntimeFunctionCountersUpdateHandler) {                        \
        auto oldGlobalMode = setGlobalRuntimeFunctionCountersMode(0);          \
        auto oldPerObjectMode = setPerObjectRuntimeFunctionCountersMode(0);    \
        GlobalRuntimeFunctionCountersUpdateHandler(                            \
            object, RT_FUNCTION_ID(RT_FUNCTION));                              \
        setGlobalRuntimeFunctionCountersMode(oldGlobalMode);                   \
        setPerObjectRuntimeFunctionCountersMode(oldPerObjectMode);             \
      }                                                                        \
    }                                                                          \
    /* Update per object counters. */                                          \
    if (UpdatePerObjectRuntimeFunctionCounters && object) {                    \
      RuntimeObjectStateCache[object]                                          \
          .SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION)++;           \
      /* TODO: Remember the order/history of operations? */                    \
    }                                                                          \
  }
#include "RuntimeInvocationsTracking.def"

/// Public APIs

/// Get the runtime object state associated with an object.
SWIFT_RT_ENTRY_VISIBILITY
void getObjectRuntimeFunctionCounters(HeapObject *object,
                                      RuntimeFunctionCountersState *result) {
  *result = RuntimeObjectStateCache[object];
}

/// Set the runtime object state associated with an object from a provided
/// state.
SWIFT_RT_ENTRY_VISIBILITY
void setObjectRuntimeFunctionCounters(HeapObject *object,
                                      RuntimeFunctionCountersState *state) {
  RuntimeObjectStateCache[object] = *state;
}

/// Get the global runtime state containing the total numbers of invocations for
/// each runtime function of interest.
SWIFT_RT_ENTRY_VISIBILITY
void getGlobalRuntimeFunctionCounters(RuntimeFunctionCountersState *result) {
  *result = RuntimeGlobalFunctionCountersState;
}

/// Set the global runtime state of function pointers from a provided state.
SWIFT_RT_ENTRY_VISIBILITY
void setGlobalRuntimeFunctionCounters(RuntimeFunctionCountersState *state) {
  RuntimeGlobalFunctionCountersState = *state;
}

/// Return the names of the runtime functions being tracked.
/// Their order is the same as the order of the counters in the
/// RuntimeObjectState structure. All these strings are null terminated.
SWIFT_RT_ENTRY_VISIBILITY
const char **getRuntimeFunctionNames() {
  return RuntimeFunctionNames;
}

/// Return the offsets of the runtime function counters being tracked.
/// Their order is the same as the order of the counters in the
/// RuntimeObjectState structure.
SWIFT_RT_ENTRY_VISIBILITY
const uint16_t *getRuntimeFunctionCountersOffsets() {
  return RuntimeFunctionCountersOffsets;
}

/// Return the number of runtime functions being tracked.
SWIFT_RT_ENTRY_VISIBILITY
uint64_t getNumRuntimeFunctionCounters() {
  return ID_LastRuntimeFunctionName;
}

static void dumpRuntimeCounters(RuntimeFunctionCountersState *State) {
  uint32_t tmp;
/// Define how to dump the counter for a given runtime function.
#define FUNCTION_TO_TRACK(RT_FUNCTION)                                         \
  tmp = State->SWIFT_RT_FUNCTION_INVOCATION_COUNTER_NAME(RT_FUNCTION);         \
  if (tmp != 0)                                                                \
    printf("%s = %d\n",                                                        \
           RuntimeFunctionNames[(int)RT_FUNCTION_ID(RT_FUNCTION)], tmp);
#include "RuntimeInvocationsTracking.def"
}

/// Dump all per-object runtime function pointers.
SWIFT_RT_ENTRY_VISIBILITY
void dumpObjectsRuntimeFunctionPointers() {
  for (auto &Pair : RuntimeObjectStateCache) {
    printf("\n\nRuntime counters for object at address %p:\n", Pair.getFirst());
    dumpRuntimeCounters(&Pair.getSecond());
    printf("\n");
  }
}

/// Set mode for global runtime function counters.
/// Return the old value of this flag.
SWIFT_RT_ENTRY_VISIBILITY
int setGlobalRuntimeFunctionCountersMode(int mode) {
  int oldMode = UpdateGlobalRuntimeFunctionCounters;
  UpdateGlobalRuntimeFunctionCounters = mode ? 1 : 0;
  return oldMode;
}

/// Set mode for per object runtime function counters.
/// Return the old value of this flag.
SWIFT_RT_ENTRY_VISIBILITY
int setPerObjectRuntimeFunctionCountersMode(int mode) {
  int oldMode = UpdatePerObjectRuntimeFunctionCounters;
  UpdatePerObjectRuntimeFunctionCounters = mode ? 1 : 0;
  return oldMode;
}

/// Add the ability to call custom handlers when a counter
/// is being updated. The handler should take the object and may be
/// the name of the runtime function as parameters. And this handler
/// could e.g. check some conditions and stop the program under
/// a debuggger if a certain condition is met, like a refcount has
/// reached a certain value.
/// We could allow for setting global handlers or even per-object
/// handlers.
SWIFT_RT_ENTRY_VISIBILITY
RuntimeFunctionCountersUpdateHandler
setGlobalRuntimeFunctionCountersUpdateHandler(
    RuntimeFunctionCountersUpdateHandler handler) {
  auto oldHandler = GlobalRuntimeFunctionCountersUpdateHandler;
  GlobalRuntimeFunctionCountersUpdateHandler = handler;
  return oldHandler;
}

/// TODO: Provide an API to remove any counters releated to a specific object
/// or all objects.
/// This is useful if you want to reset the stats for some/all objects.
