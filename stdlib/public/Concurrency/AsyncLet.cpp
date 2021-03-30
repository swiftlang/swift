//===--- Task.cpp - Task object and management ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Object management routines for asynchronous task objects.
//
//===----------------------------------------------------------------------===//

#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/AsyncLet.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/HeapObject.h"
#include "TaskPrivate.h"
#include "AsyncCall.h"
#include "Debug.h"

#if !defined(_WIN32)
#include <dlfcn.h>
#endif

using namespace swift;

namespace {
class AsyncLetImpl: public ChildTaskStatusRecord {
public:
  /// Describes the status of the async let.
  enum class Status : uintptr_t {
//    /// True if the `async let` declared variable was awaited on at least once.
//    /// This means that subsequent asyncLetGet operations need not suspend.
//    HasBeenAwaited = 0b01,
  };

private:

  /// The task that was kicked off to initialize this `async let`.
  AsyncTask *task;

  // TODO: more additional flags here, we can use them for future optimizations.

  friend class AsyncTask;

public:
  explicit AsyncLetImpl(AsyncTask* task)
      : ChildTaskStatusRecord(task),
        task(task) {
    assert(task->hasChildFragment() && "async let task must be a child task.");
  }

  ChildTaskStatusRecord *getTaskRecord() {
    return reinterpret_cast<ChildTaskStatusRecord *>(this);
  }

  bool wasAwaitedOn() const {
    return false; // FIXME: implement this
  }

  AsyncTask *getTask() const {
    return task;
  }

  Status *getStatus() const {
    assert(false);
  }

}; // end AsyncLetImpl

} // end anonymous namespace


/******************************************************************************/
/************************* ASYNC LET IMPLEMENTATION ***************************/
/******************************************************************************/

static_assert(sizeof(AsyncLetImpl) <= sizeof(AsyncLet) &&
              alignof(AsyncLetImpl) <= alignof(AsyncLet),
              "AsyncLetImpl doesn't fit in AsyncLet");

static AsyncLetImpl *asImpl(AsyncLet *alet) {
  return reinterpret_cast<AsyncLetImpl*>(alet);
}

static AsyncLetImpl *asImpl(const AsyncLet *alet) {
  return reinterpret_cast<AsyncLetImpl*>(
      const_cast<AsyncLet*>(alet));
}

static AsyncLet *asAbstract(AsyncLetImpl *alet) {
  return reinterpret_cast<AsyncLet*>(alet);
}

// =============================================================================
// ==== initialize -------------------------------------------------------------

SWIFT_CC(swift)
static void swift_asyncLet_initializeImpl(AsyncLet *alet, AsyncTask *task) {
  swift_retain(task);
  AsyncLetImpl *impl = new (alet) AsyncLetImpl(task);

  auto record = impl->getTaskRecord();
  assert(impl == record && "the async-let IS the task record");

  // ok, now that the group actually is initialized: attach it to the task
  swift_task_addStatusRecord(record);
}

// =============================================================================
// ==== create -----------------------------------------------------------------

// TODO: remove and replace by calling initialize directly from SILGenDecl
//       this way we can allocate in SIL and pass the storage into initialize
SWIFT_CC(swift)
static AsyncLet *swift_asyncLet_createImpl(AsyncTask *task) {
//  void *allocation = swift_task_alloc(sizeof(AsyncLet));
  void *allocation = malloc(sizeof(AsyncLet));
  auto alet = reinterpret_cast<AsyncLet *>(allocation);
  swift_asyncLet_initialize(alet, task);
  return alet;
}

// =============================================================================
// ==== wait -------------------------------------------------------------------

SWIFT_CC(swiftasync)
static void swift_asyncLet_waitImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *rawContext,
    AsyncLet *alet, Metadata *T) {
  auto task = alet->getTask();
  swift_task_future_wait(result, rawContext, task, T);
}

SWIFT_CC(swiftasync)
static void swift_asyncLet_wait_throwingImpl(
    OpaqueValue *result, SWIFT_ASYNC_CONTEXT AsyncContext *rawContext,
    AsyncLet *alet, Metadata *T) {
  auto task = alet->getTask();
  swift_task_future_wait_throwing(result, rawContext, task, T);
}

// =============================================================================
// ==== end --------------------------------------------------------------------

SWIFT_CC(swift)
static void swift_asyncLet_endImpl(AsyncLet *alet) {
  auto task = alet->getTask();
//  assert(alet->wasAwaitedOn() && "async let was never awaited yet is about to be destroyed!");

  // Remove the child record from the parent task
  auto record = asImpl(alet)->getTaskRecord();
  swift_task_removeStatusRecord(record);

  // and finally, release the task and free the async-let
  swift_release(task);
  free(alet);
}

// =============================================================================
// ==== AsyncLet Implementation ------------------------------------------------

bool AsyncLet::wasAwaitedOn() const {
  return asImpl(this)->wasAwaitedOn();
}

AsyncTask* AsyncLet::getTask() const {
  return asImpl(this)->getTask();
}

// =============================================================================

#define OVERRIDE_ASYNC_LET COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
