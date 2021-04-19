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

#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "swift/ABI/TaskLocal.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"
#include "TaskPrivate.h"

using namespace swift;

// =============================================================================
// ==== ABI --------------------------------------------------------------------

SWIFT_CC(swift)
static void swift_task_localValuePushImpl(AsyncTask *task,
                                          const HeapObject *key,
                                          /* +1 */ OpaqueValue *value,
                                          const Metadata *valueType) {
  task->localValuePush(key, value, valueType);
}

SWIFT_CC(swift)
static OpaqueValue* swift_task_localValueGetImpl(AsyncTask *task,
                                                 const HeapObject *key,
                                                 TaskLocal::TaskLocalInheritance inheritance) {
  return task->localValueGet(key, inheritance);
}

SWIFT_CC(swift)
static void swift_task_localValuePopImpl(AsyncTask *task) {
  task->localValuePop();
}

SWIFT_CC(swift)
static void destroyTaskLocalHeapItem(SWIFT_CONTEXT HeapObject *obj) {
  fprintf(stderr, "[%s:%d] (%s) DESTROY heapItem:%p\n", __FILE__, __LINE__, __FUNCTION__, obj);
//  assert(false && "TODO: we got released and should destroy it now");
//  TaskLocal::HeapItem *heapItem = static_cast<TaskLocal::HeapItem*>(obj);
}

// =============================================================================
// ==== TaskLocal::HeapItem Metadata -------------------------------------------

/// Heap metadata for an asynchronous task.
FullMetadata<HeapMetadata> swift::taskLocalHeapItemHeapMetadata = {
    {
        {
            &destroyTaskLocalHeapItem
        },
        {
            /*value witness table*/ nullptr
        }
    },
    {
        MetadataKind::TaskLocalHeapItem
    }
};

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
  fprintf(stderr, "[%s:%d] (%s) CREATE PARENT LINK\n", __FILE__, __LINE__, __FUNCTION__);

  size_t amountToAllocate = Item::itemSize(/*isHeapItem*/false, /*valueType*/nullptr);
  // assert(amountToAllocate % MaximumAlignment == 0); // TODO: do we need this?
  void *allocation = _swift_task_alloc_specific(task, amountToAllocate);
  Item *item = new(allocation) Item();

  // FIXME: parent pointer must to be the parent STORAGE not just the next item.
  auto parentHead = parent->Local.head;
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
        case NextLinkType::ThisItemIsHeapItem:
          assert(false && "ThisItemIsHeapItem?"); // FIXME: what?
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
  assert(task);
  fprintf(stderr, "[%s:%d] (%s) CREATE LINK, key:%p\n", __FILE__, __LINE__, __FUNCTION__, key);

  // If we're executing this inside of a withTaskGroup { ... } we risk the
  // following situation:
  //
  //    withTaskGroup(...) { group in // (Parent Task):[,...]
  //      // the TaskGroupRecord is present throughout the execution of this closure
  //      Lib.$key.withValue("xoxo") { // (Parent Task)[key="xoxo",...]
  //        group.spawn {
  //          <sleep>
  //          print(Lib.someTaskLocal)
  //        } //
  //      }
  //      return group.next()!
  //    }
  bool isStructured = !swift_task_hasTaskGroupStatusRecord();
  bool isHeapItem = !isStructured;

  size_t amountToAllocate = Item::itemSize(isHeapItem, valueType);
  // assert(amountToAllocate % MaximumAlignment == 0); // TODO: do we need this?

  void *allocation;
  Item *item = nullptr;
  HeapItem *heapItem = nullptr;

  if (isStructured) {
    allocation = _swift_task_alloc_specific(task, amountToAllocate);
    fprintf(stderr, "[%s:%d] (%s) fast-path, TASK_ALLOC, key:%p, ptr:%p\n",
            __FILE__, __LINE__, __FUNCTION__, key, allocation);
    item = new(allocation) Item(key, valueType);
  } else {
    allocation = malloc(amountToAllocate);
    fprintf(stderr, "[%s:%d] (%s) slow-path, unstructured, MALLOC, key:%p, ptr:%p\n", __FILE__, __LINE__, __FUNCTION__, key, allocation);
    heapItem = new(allocation) HeapItem(key, valueType);
    fprintf(stderr, "[%s:%d] (%s) slow-path, unstructured; heapItem:%p\n", __FILE__, __LINE__, __FUNCTION__, heapItem);
    item = heapItem->getItem();
    fprintf(stderr, "[%s:%d] (%s) slow-path, unstructured; item:%p\n", __FILE__, __LINE__, __FUNCTION__, item);

    fprintf(stderr, "[%s:%d] (%s) BEFORE heapItem:%p, ref-count:%d\n", __FILE__, __LINE__, __FUNCTION__, heapItem, swift_retainCount(heapItem));
    swift_retain(heapItem);
    fprintf(stderr, "[%s:%d] (%s) AFTER heapItem:%p, ref-count:%d\n", __FILE__, __LINE__, __FUNCTION__, heapItem, swift_retainCount(heapItem));
  }


  auto next = task->Local.head;
  auto nextLinkType = isStructured ? NextLinkType::IsNext // FIXME: don't really need the isNext, only is parent and heap item need markers
                                   : NextLinkType::ThisItemIsHeapItem;
  item->next = reinterpret_cast<uintptr_t>(next) |
               static_cast<uintptr_t>(nextLinkType);

  return item;


//
//  Item *item = new(allocation) Item(key, valueType);
//
//  auto next = task->Local.head;
////  auto nextLinkType = next ? NextLinkType::IsNext
////                           : NextLinkType::IsTerminal;
//  auto nextLinkType = isStructured ? NextLinkType::IsNext
//                                   : NextLinkType::ThisItemIsHeapItem;
//  item->next = reinterpret_cast<uintptr_t>(next) |
//               static_cast<uintptr_t>(nextLinkType);
//
//  return item;
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void TaskLocal::Item::destroy(AsyncTask *task) {
  auto linkType = getNextLinkType();

  // if it was unstructured, we cannot delete it until all references are gone
  if (linkType == TaskLocal::NextLinkType::ThisItemIsHeapItem) {
    fprintf(stderr, "[%s:%d] (%s) destroy item:%p\n", __FILE__, __LINE__, __FUNCTION__, this);
    assert(false && "implement releasing the heap item");
    return;
  }

  // otherwise it was task-local allocated, so we can safely destroy it right away
  if (valueType) {
    valueType->vw_destroy(getStoragePtr());
  }

  _swift_task_dealloc_specific(task, this);
}

void TaskLocal::Storage::destroy(AsyncTask *task) {
  auto item = head;
  head = nullptr;
  TaskLocal::Item *next;
  while (item) {
    auto linkType = item->getNextLinkType();
    switch (linkType) {
    case TaskLocal::NextLinkType::ThisItemIsHeapItem: {
      fprintf(stderr, "[%s:%d] (%s) destroy UNSTRUCTURED\n", __FILE__, __LINE__, __FUNCTION__);
      LLVM_FALLTHROUGH;
    }
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
// ==== push / pop / get -------------------------------------------------------

void TaskLocal::Storage::pushValue(AsyncTask *task,
                                   const HeapObject *key,
                                   /* +1 */ OpaqueValue *value,
                                   const Metadata *valueType) {
  assert(value && "Task local value must not be nil");

  auto item = Item::createLink(task, key, valueType);
  fprintf(stderr, "[%s:%d] (%s) here\n", __FILE__, __LINE__, __FUNCTION__);
  valueType->vw_initializeWithTake(item->getStoragePtr(), value);
  head = item;
}

void TaskLocal::Storage::popValue(AsyncTask *task) {
  assert(head && "attempted to pop value off empty task-local stack");
  auto old = head;
  head = head->getNext();
  old->destroy(task);
}

OpaqueValue* TaskLocal::Storage::getValue(AsyncTask *task,
                                          const HeapObject *key,
                                          const TaskLocalInheritance inherit) {
  assert(key && "TaskLocal key must not be null.");

  auto item = head;
  while (item) {
    if (item->key == key) {
      return item->getStoragePtr();
    }

    // if the key is an `inherit = .never` type, we stop our search the first
    // time we would be jumping to a parent task to continue the search.
    if (item->getNextLinkType() == NextLinkType::IsParent &&
        inherit == TaskLocalInheritance::Never)
      return nullptr;

    item = item->getNext();
  }

  return nullptr;
}

#define OVERRIDE_TASK_LOCAL COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
