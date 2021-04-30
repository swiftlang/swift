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
#include "swift/ABI/Actor.h"
#include "swift/ABI/Task.h"
#include "swift/ABI/Metadata.h"
#include "swift/Runtime/Once.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Concurrency.h"
#include "TaskPrivate.h"

#if defined(__APPLE__)
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

#if defined(_WIN32)
#include <io.h>
#endif

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
                                                 const HeapObject *key) {
  return task->localValueGet(key);
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

SWIFT_CC(swift)
static void swift_task_reportIllegalTaskLocalBindingWithinWithTaskGroupImpl(
    const unsigned char *file, uintptr_t fileLength,
    bool fileIsASCII, uintptr_t line) {

  char *message;
  swift_asprintf(
      &message,
      "error: task-local: detected illegal task-local value binding at %.*s:%d.\n"
      "Task-local values must only be set in a structured-context, such as: "
      "around any (synchronous or asynchronous function invocation), "
      "around an 'async let' declaration, or around a 'with(Throwing)TaskGroup(...){ ... }' "
      "invocation. Notably, binding a task-local value is illegal *within the body* "
      "of a withTaskGroup invocation.\n"
      "\n"
      "The following example is illegal:\n\n"
      "    await withTaskGroup(...) { group in \n"
      "        await <task-local>.withValue(1234) {\n"
      "            group.spawn { ... }\n"
      "        }\n"
      "    }\n"
      "\n"
      "And should be replaced by, either: setting the value for the entire group:\n"
      "\n"
      "    // bind task-local for all tasks spawned within the group\n"
      "    await <task-local>.withValue(1234) {\n"
      "        await withTaskGroup(...) { group in\n"
      "            group.spawn { ... }\n"
      "        }\n"
      "    }\n"
      "\n"
      "or, inside the specific task-group child task:\n"
      "\n"
      "    // bind-task-local for only specific child-task\n"
      "    await withTaskGroup(...) { group in\n"
      "        group.spawn {\n"
      "            await <task-local>.withValue(1234) {\n"
      "                ... \n"
      "            }\n"
      "        }\n"
      "\n"
      "        group.spawn { ... }\n"
      "    }\n",
      (int)fileLength, file,
      (int)line);

  if (_swift_shouldReportFatalErrorsToDebugger()) {
    RuntimeErrorDetails details = {
        .version = RuntimeErrorDetails::currentVersion,
        .errorType = "task-local-violation",
        .currentStackDescription = "Task-local bound in illegal context",
        .framesToSkip = 1,
    };
    _swift_reportToDebugger(RuntimeErrorFlagFatal, message, &details);
  }

#if defined(_WIN32)
  #define STDERR_FILENO 2
  _write(STDERR_FILENO, message, strlen(message));
#else
  write(STDERR_FILENO, message, strlen(message));
#endif
#if defined(__APPLE__)
  asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", message);
#elif defined(__ANDROID__)
  __android_log_print(ANDROID_LOG_FATAL, "SwiftRuntime", "%s", message);
#endif

  free(message);
  abort();
}

TaskLocal::Item*
TaskLocal::Item::createLink(AsyncTask *task,
                        const HeapObject *key,
                        const Metadata *valueType) {
  assert(task);

  size_t amountToAllocate = Item::itemSize(valueType);
  // assert(amountToAllocate % MaximumAlignment == 0); // TODO: do we need this?

  void *allocation = _swift_task_alloc_specific(task, amountToAllocate);
  Item *item = new(allocation) Item(key, valueType);

  auto next = task->Local.head;
  item->next = reinterpret_cast<uintptr_t>(next) |
      static_cast<uintptr_t>(NextLinkType::IsNext);

  return item;
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

void TaskLocal::Item::destroy(AsyncTask *task) {
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

#define OVERRIDE_TASK_LOCAL COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
