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

extern "C" SWIFT_CC(swift) void swift_task_checkIsolatedImpl(
    SerialExecutorRef executor) {
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

extern "C" SWIFT_CC(swift) int8_t
swift_task_isIsolatingCurrentContextImpl(
    SerialExecutorRef executor) {
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

extern "C" SWIFT_CC(swift) bool swift_task_isMainExecutorImpl(
    SerialExecutorRef executor) {
  HeapObject *identity = executor.getIdentity();
  return executor.hasSerialExecutorWitnessTable() &&
         _swift_task_isMainExecutorSwift(
             identity, swift_getObjectType(identity),
             executor.getSerialExecutorWitnessTable());
}
