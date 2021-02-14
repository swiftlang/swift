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
        free(item);
        item = next;
        break;

      case TaskLocalValuesFragment::NextLinkType::IsParent:
      case TaskLocalValuesFragment::NextLinkType::IsTerminal:
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
  head = item;
}

void TaskLocalValuesFragment::popValue(AsyncTask *task) {
  assert(head && "attempted to pop value off empty task-local stack");
  head->destroy();
  head = head->getNext();
}

OpaqueValue *TaskLocalValuesFragment::get(
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

