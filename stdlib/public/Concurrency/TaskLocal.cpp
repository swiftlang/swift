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
                                          const Metadata *keyType,
                                          /* +1 */ OpaqueValue *value,
                                          const Metadata *valueType) {
  task->localValuePush(keyType, value, valueType);
}

SWIFT_CC(swift)
static OpaqueValue* swift_task_localValueGetImpl(AsyncTask *task,
                                                 const Metadata *keyType,
                                                 TaskLocal::TaskLocalInheritance inheritance) {
  return task->localValueGet(keyType, inheritance);
}

SWIFT_CC(swift)
static void swift_task_localValuePopImpl(AsyncTask *task) {
  task->localValuePop();
}

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
  // assert(amountToAllocate % MaximumAlignment == 0); // TODO: do we need this?
  void *allocation = _swift_task_alloc_specific(task, amountToAllocate);
  Item *item = new(allocation) Item();

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
          assert(false &&
                 "empty taskValue head in parent task, yet parent's 'head' is `IsNext`, "
                 "this should not happen, as it implies the parent must have stored some value.");
          break;
        case NextLinkType::IsTerminal:
          item->next = reinterpret_cast<uintptr_t>(parentHead->getNext()) |
                       static_cast<uintptr_t>(NextLinkType::IsTerminal);
          break;
      }
    } else {
      item->next = reinterpret_cast<uintptr_t>(parentHead) |
                   static_cast<uintptr_t>(NextLinkType::IsParent);
    }
  } else {
    item->next = reinterpret_cast<uintptr_t>(parentHead) |
                 static_cast<uintptr_t>(NextLinkType::IsTerminal);
  }

  return item;
}

TaskLocal::Item*
TaskLocal::Item::createLink(AsyncTask *task,
                        const Metadata *keyType,
                        const Metadata *valueType) {
  assert(task);
  size_t amountToAllocate = Item::itemSize(valueType);
  // assert(amountToAllocate % MaximumAlignment == 0); // TODO: do we need this?
  void *allocation = _swift_task_alloc_specific(task, amountToAllocate);
  Item *item = new(allocation) Item(keyType, valueType);

  auto next = task->Local.head;
  auto nextLinkType = next ? NextLinkType::IsNext
                           : NextLinkType::IsTerminal;
  item->next = reinterpret_cast<uintptr_t>(next) |
               static_cast<uintptr_t>(nextLinkType);

  return item;
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void TaskLocal::Item::destroy(AsyncTask *task) {
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
    switch (item->getNextLinkType()) {
      case TaskLocal::NextLinkType::IsNext:
        next = item->getNext();
        item->destroy(task);
        item = next;
        break;

      case TaskLocal::NextLinkType::IsParent:
      case TaskLocal::NextLinkType::IsTerminal:
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
                                   const Metadata *keyType,
                                   /* +1 */ OpaqueValue *value,
                                   const Metadata *valueType) {
  assert(value && "Task local value must not be nil");

  auto item = Item::createLink(task, keyType, valueType);
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
                                          const Metadata *keyType,
                                          const TaskLocalInheritance inherit) {
  assert(keyType && "Task.Local key must not be null.");

  auto item = head;
  while (item) {
    if (item->keyType == keyType) {
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
