//===--- TaskLocal.cpp - Task Local Values --------------------------------===//
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

#include <cstdint>

#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Portability.h"

#include "Concurrency/Threading/Mutex.h"
#include "Runtime/Threading/ThreadLocal.h"
#include "Runtime/Concurrency.h"

#include "Concurrency/TaskLocal.h"
#include "Concurrency/Task.h"
#include "Concurrency/Actor.h"
/* #include "Concurrency/Metadata.h" */

#include "Concurrency/TaskPrivate.h"

#include "llvm/ADT/PointerIntPair.h"
/* #include "TaskPrivate.h" */
#include <set>

#if defined(__APPLE__)
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

#if HAVE_PTHREAD_H
#include <pthread.h>
#endif

#if defined(_WIN32)
#include <io.h>
#include <handleapi.h>
#include <processthreadsapi.h>
#endif

using namespace swift;

// =============================================================================

/// An extremely silly class which exists to make pointer
/// default-initialization constexpr.
template <class T> struct Pointer {
  T *Value;
  constexpr Pointer() : Value(nullptr) {}
  constexpr Pointer(T *value) : Value(value) {}
  operator T *() const { return Value; }
  T *operator->() const { return Value; }
};

/// THIS IS RUNTIME INTERNAL AND NOT ABI.
class FallbackTaskLocalStorage {
  static SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
      Pointer<TaskLocal::Storage>, Value,
      SWIFT_CONCURRENCY_FALLBACK_TASK_LOCAL_STORAGE_KEY);

public:
  static void set(TaskLocal::Storage *task) { Value.set(task); }
  static TaskLocal::Storage *get() { return Value.get(); }
};

/// Define the thread-locals.
SWIFT_RUNTIME_DECLARE_THREAD_LOCAL(
    Pointer<TaskLocal::Storage>, FallbackTaskLocalStorage::Value,
    SWIFT_CONCURRENCY_FALLBACK_TASK_LOCAL_STORAGE_KEY);

// =============================================================================
// ==== Initialization ---------------------------------------------------------

void TaskLocal::Storage::initializeLinkParent(AsyncTask* task,
                                              AsyncTask* parent) {
  assert(!head && "initial task local storage was already initialized");
  assert(parent && "parent must be provided to link to it");
  head = TaskLocal::Item::createParentLink(task, parent);
}

TaskLocal::Item*
TaskLocal::Item::createParentLink(AsyncTask *task, AsyncTask *parent) {
  size_t amountToAllocate = Item::itemSize(/*valueType*/nullptr);
  void *allocation = _swift_task_alloc_specific(task, amountToAllocate);
  Item *item = new(allocation) Item();

  auto parentHead = parent->_private().Local.head;
  if (parentHead) {
    if (parentHead->isEmpty()) {
      switch (parentHead->getNextLinkType()) {
        case NextLinkType::IsParent:
          // it has no values, and just points to its parent,
          // therefore skip also skip pointing to that parent and point
          // to whichever parent it was pointing to as well, it may be its
          // immediate parent, or some super-parent.
          item->next = reinterpret_cast<uintptr_t>(parentHead->getNext()) |
                       static_cast<uintptr_t>(NextLinkType::IsParent);
          break;
        case NextLinkType::IsNext:
          if (parentHead->getNext()) {
            assert(false && "empty taskValue head in parent task, yet parent's 'head' is `IsNext`, "
                            "this should not happen, as it implies the parent must have stored some value.");
          } else {
            // is terminal pointer
            item->next = reinterpret_cast<uintptr_t>(parentHead->getNext());
          }
          break;
      }
    } else {
      item->next = reinterpret_cast<uintptr_t>(parentHead) |
                   static_cast<uintptr_t>(NextLinkType::IsParent);
    }
  } else {
    item->next = reinterpret_cast<uintptr_t>(parentHead);
  }

  return item;
}

TaskLocal::Item*
TaskLocal::Item::createLink(AsyncTask *task,
                            const HeapObject *key,
                            const Metadata *valueType) {
  size_t amountToAllocate = Item::itemSize(valueType);
  void *allocation = task ? _swift_task_alloc_specific(task, amountToAllocate)
                          : malloc(amountToAllocate);
  Item *item = new (allocation) Item(key, valueType);

  auto next = task ? task->_private().Local.head
                   : FallbackTaskLocalStorage::get()->head;
  item->next = reinterpret_cast<uintptr_t>(next) |
      static_cast<uintptr_t>(NextLinkType::IsNext);

  return item;
}


void TaskLocal::Item::copyTo(AsyncTask *target) {
  assert(target && "TaskLocal item attempt to copy to null target task!");

  // 'parent' pointers are signified by null valueType.
  // We must not copy parent pointers, but rather perform a deep copy of all values,
  // as such, we skip parent pointers here entirely.
  if (isParentPointer())
    return;

  auto item = Item::createLink(target, key, valueType);
  valueType->vw_initializeWithCopy(item->getStoragePtr(), getStoragePtr());

  /// A `copyTo` may ONLY be invoked BEFORE the task is actually scheduled,
  /// so right now we can safely copy the value into the task without additional
  /// synchronization.
  target->_private().Local.head = item;
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void TaskLocal::Item::destroy(AsyncTask *task) {
  // otherwise it was task-local allocated, so we can safely destroy it right away
  if (valueType) {
    valueType->vw_destroy(getStoragePtr());
  }

  // if task is available, we must have used the task allocator to allocate this item,
  // so we must deallocate it using the same. Otherwise, we must have used malloc.
  if (task) _swift_task_dealloc_specific(task, this);
  else free(this);
}

void TaskLocal::Storage::destroy(AsyncTask *task) {
  auto item = head;
  head = nullptr;
  TaskLocal::Item *next;
  while (item) {
    auto linkType = item->getNextLinkType();
    switch (linkType) {
    case TaskLocal::NextLinkType::IsNext:
        next = item->getNext();
        item->destroy(task);
        item = next;
        break;

      case TaskLocal::NextLinkType::IsParent:
        // we're done here; as we must not proceed into the parent owned values.
        // we do have to destroy the item pointing at the parent/edge itself though.
        item->destroy(task);
        return;
    }
  }
}

// =============================================================================
// ==== Task Local Storage: operations -----------------------------------------

void TaskLocal::Storage::pushValue(AsyncTask *task,
                                   const HeapObject *key,
                                   /* +1 */ OpaqueValue *value,
                                   const Metadata *valueType) {
  assert(value && "Task local value must not be nil");

  auto item = Item::createLink(task, key, valueType);
  valueType->vw_initializeWithTake(item->getStoragePtr(), value);
  head = item;
}

bool TaskLocal::Storage::popValue(AsyncTask *task) {
  assert(head && "attempted to pop value off empty task-local stack");
  auto old = head;
  head = head->getNext();
  old->destroy(task);

  /// if pointing at not-null next item, there are remaining bindings.
  return head != nullptr;
}

OpaqueValue* TaskLocal::Storage::getValue(AsyncTask *task,
                                          const HeapObject *key) {
  assert(key && "TaskLocal key must not be null.");

  auto item = head;
  while (item) {
    if (item->key == key) {
      return item->getStoragePtr();
    }

    item = item->getNext();
  }

  return nullptr;
}


void TaskLocal::Storage::copyTo(AsyncTask *target) {
  assert(target && "task must not be null when copying values into it");
  assert(!(target->_private().Local.head) &&
      "Task must not have any task-local values bound before copying into it");

  // Set of keys for which we already have copied to the new task.
  // We only ever need to copy the *first* encounter of any given key,
  // because it is the most "specific"/"recent" binding and any other binding
  // of a key does not matter for the target task as it will never be able to
  // observe it.
  std::set<const HeapObject*> copied = {};

  auto item = head;
  while (item) {
    // we only have to copy an item if it is the most recent binding of a key.
    // i.e. if we've already seen an item for this key, we can skip it.
    if (copied.emplace(item->key).second) {
      item->copyTo(target);
    }

    item = item->getNext();
  }
}
