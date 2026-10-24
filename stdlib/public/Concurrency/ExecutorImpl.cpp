//===--- ExecutorImpl.cpp - C++ side of executor impl ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_CONCURRENCY_USES_DISPATCH
#include <dispatch/dispatch.h>
#endif

#include "Error.h"
#include "ExecutorBridge.h"
#include "swift/Runtime/Casting.h"

#if SWIFT_CONCURRENCY_EMBEDDED
#include "ExecutorImpl.h"
#include "swift/EmbeddedPlatform.h"
#endif

using namespace swift;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"


extern "C" SWIFT_CC(swift)
void _swift_task_checkIsolatedSwift(
  HeapObject *executor,
  const Metadata *executorType,
  const SerialExecutorWitnessTable *witnessTable
);

extern "C" SWIFT_CC(swift) bool _swift_task_isMainExecutorSwift(
    HeapObject *executor, const Metadata *executorType,
    const SerialExecutorWitnessTable *witnessTable);

static void checkIsolated(SerialExecutorRef executor) {
  HeapObject *identity = executor.getIdentity();

  // We might be being called with an actor rather than a "proper"
  // SerialExecutor; in that case, we won't have a SerialExecutor witness
  // table.
  if (executor.hasSerialExecutorWitnessTable()) {
    _swift_task_checkIsolatedSwift(identity, swift_getObjectType(identity),
                                   executor.getSerialExecutorWitnessTable());
  } else {
    const Metadata *objectType = swift_getObjectType(executor.getIdentity());
    auto typeName = swift_getTypeName(objectType, true);

    swift_Concurrency_fatalError(
        0, "Incorrect actor executor assumption; expected '%.*s' executor.\n",
        (int)typeName.length, typeName.data);
  }
}


extern "C" SWIFT_CC(swift)
int8_t _swift_task_isIsolatingCurrentContextSwift(
  HeapObject *executor,
  const Metadata *executorType,
  const SerialExecutorWitnessTable *witnessTable
);

static int8_t isIsolatingCurrentContext(SerialExecutorRef executor) {
  HeapObject *identity = executor.getIdentity();

  // We might be being called with an actor rather than a "proper"
  // SerialExecutor; in that case, we won't have a SerialExecutor witness
  // table.
  if (!executor.hasSerialExecutorWitnessTable())
    return static_cast<uint8_t>(IsIsolatingCurrentContextDecision::Unknown);

  return _swift_task_isIsolatingCurrentContextSwift(
      identity, swift_getObjectType(identity),
      executor.getSerialExecutorWitnessTable());
}

#if SWIFT_CONCURRENCY_EMBEDDED

// The executor entrypoints of EmbeddedPlatform.h default to the executor in
// ExecutorImpl.swift. libswift_ConcurrencyDefaultExecutor.a replaces them when
// linked after this archive only because no default exists here for the delay
// and deadline entrypoints, which the hooks reference and only its executor
// defines.

extern "C" __attribute__((weak))
void _swift_task_enqueueGlobalImpl(SwiftJob *job) {
  swift_task_enqueueGlobalImpl(job);
}

extern "C" __attribute__((weak))
void _swift_task_enqueueMainExecutorImpl(SwiftJob *job) {
  swift_task_enqueueMainExecutorImpl(job);
}

extern "C" __attribute__((weak))
SwiftExecutorRef _swift_task_getMainExecutorImpl(void) {
  return swift_task_getMainExecutorImpl();
}

extern "C" __attribute__((weak))
bool _swift_task_isMainExecutorImpl(SwiftExecutorRef executor) {
  // In the embedded cooperative executor model the main executor is the generic
  // executor, as in CooperativeGlobalExecutor.cpp. Custom SerialExecutor
  // witness tables and the _swift_task_isMainExecutorSwift bridge are
  // unavailable in embedded mode.
  return swift_executor_isGeneric(executor);
}

extern "C" __attribute__((weak))
void _swift_task_checkIsolatedImpl(SwiftExecutorRef executor) {
  checkIsolated(*reinterpret_cast<SerialExecutorRef *>(&executor));
}

extern "C" __attribute__((weak))
int8_t _swift_task_isIsolatingCurrentContextImpl(SwiftExecutorRef executor) {
  return isIsolatingCurrentContext(
      *reinterpret_cast<SerialExecutorRef *>(&executor));
}

extern "C" __attribute__((weak))
void _swift_task_asyncMainDrainQueueImpl(void) {
  swift_task_asyncMainDrainQueueImpl();
}

extern "C" __attribute__((weak))
void _swift_task_donateThreadToGlobalExecutorUntilImpl(
    bool (*condition)(void *), void *conditionContext) {
  swift_task_donateThreadToGlobalExecutorUntilImpl(condition, conditionContext);
}

#else

extern "C" SWIFT_CC(swift) void swift_task_checkIsolatedImpl(
    SerialExecutorRef executor) {
  checkIsolated(executor);
}

extern "C" SWIFT_CC(swift) int8_t
swift_task_isIsolatingCurrentContextImpl(
    SerialExecutorRef executor) {
  return isIsolatingCurrentContext(executor);
}

extern "C" SWIFT_CC(swift) bool swift_task_isMainExecutorImpl(
    SerialExecutorRef executor) {
  HeapObject *identity = executor.getIdentity();
  return executor.hasSerialExecutorWitnessTable() &&
         _swift_task_isMainExecutorSwift(
             identity, swift_getObjectType(identity),
             executor.getSerialExecutorWitnessTable());
}

#endif
