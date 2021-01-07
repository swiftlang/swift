//===--- TaskGroup.cpp - Task Group internal message channel ------------===//
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
// Object management for task local values.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"

using namespace swift;
using TaskLocalValuesFragment = AsyncTask::TaskLocalValuesFragment;

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void TaskLocalValuesFragment::destroy() {
  auto item = head;
  head = nullptr;
  TaskLocalItem *next;
  while (item) {
    switch (item->getNextLinkType()) {
      case TaskLocalValuesFragment::NextLinkType::IsNext:
        next = item->getNext();
        item->destroy();
        fprintf(stderr, "FREE: %d\n", item);
        free(item);
        item = next;
        break;

      case TaskLocalValuesFragment::NextLinkType::IsParent:
      case TaskLocalValuesFragment::NextLinkType::IsTerminal:
        fprintf(stderr, "error: %s [%s:%d] done destroying [fragment:%d] type=%d\n", __FUNCTION__, __FILE_NAME__, __LINE__,
                this, item->getNextLinkType());

        // we're done here, we must not destroy values owned by the parent task.
        return;
    }
  }
}

// =============================================================================
// ==== Initialization ---------------------------------------------------------

void TaskLocalValuesFragment::initializeLinkParent(AsyncTask* task,
                                                   AsyncTask* parent) {
  assert(!head && "fragment was already initialized");
  if (parent) {
    head = TaskLocalItem::createParentLink(task, parent);
  }
}

// =============================================================================
// ==== push / pop / get -------------------------------------------------------

void TaskLocalValuesFragment::pushValue(AsyncTask *task,
                                        const Metadata *keyType,
                                        /* +1 */ OpaqueValue *value,
                                        const Metadata *valueType) {
  assert(value && "Task local value must not be nil");

  auto item = TaskLocalItem::createLink(task, keyType, valueType);
  valueType->vw_initializeWithTake(item->getStoragePtr(), value);

  fprintf(stderr, "error: %s [%s:%d] PUSH bound item: task=%d item=%d -> item.next=%d keyType=%d item->getStoragePtr=%d\n", __FUNCTION__, __FILE_NAME__, __LINE__,
          task, item, item->getNext(), keyType, item->getStoragePtr());

  head = item;
}

void TaskLocalValuesFragment::popValue(AsyncTask *task) {
  assert(head && "attempted to pop value off empty task-local stack");
  head->destroy();
  head = head->getNext();
}

OpaqueValue *TaskLocalValuesFragment::get(const Metadata *keyType, const TaskLocalInheritance inherit) {
  assert(keyType && "Task.Local key must not be null.");

  auto item = head;
  fprintf(stderr, "error: %s [%s:%d] get @ [%d] keyType %d\n", __FUNCTION__, __FILE_NAME__, __LINE__,
          item, keyType);
  while (item) {
    fprintf(stderr, "error: %s [%s:%d] loop, get; keyType=%d item=%d value=%d\n", __FUNCTION__, __FILE_NAME__, __LINE__,
            keyType, item, item->getStoragePtr());
    if (item->keyType == keyType) {
      return item->getStoragePtr();
    }

    // if the hey is an `inherit = .never` type, we stop our search the first
    // time we would be jumping to a parent task to continue the search.
    if (item->getNextLinkType() == NextLinkType::IsParent &&
        inherit == TaskLocalInheritance::never)
      return nullptr;

    item = item->getNext();
  }

  fprintf(stderr, "error: %s [%s:%d] not found, get @ [%d] keyType %d\n", __FUNCTION__, __FILE_NAME__, __LINE__,
          item, keyType);

  return nullptr;
}

