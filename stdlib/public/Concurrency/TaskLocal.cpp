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

#include "TaskLocal.h"
#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "TaskPrivate.h"
#include "swift/ABI/Actor.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/Runtime/Atomic.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Heap.h"
#include "swift/Threading/ThreadLocalStorage.h"
#include "llvm/ADT/PointerIntPair.h"
#include <new>
#include <set>

#if SWIFT_STDLIB_HAS_ASL
#include <asl.h>
#elif defined(__ANDROID__)
#include <android/log.h>
#endif

#if defined(_WIN32)
#include <io.h>
#endif

using namespace swift;

#if 0
#define SWIFT_TASK_LOCAL_DEBUG_LOG_ENABLED 1
#define SWIFT_TASK_LOCAL_DEBUG_LOG(key, fmt, ...)                       \
fprintf(stderr, "[%s:%d][task:%p key:%p] (%s) " fmt "\n",               \
      __FILE__, __LINE__,                                               \
      swift_task_getCurrent(),                                          \
      key,                                                              \
      __FUNCTION__,                                                     \
      __VA_ARGS__)

#else
#define SWIFT_TASK_LOCAL_DEBUG_LOG_ENABLED 0
#define SWIFT_TASK_LOCAL_DEBUG_LOG(key, fmt, ...) (void)0
#endif

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
  static SWIFT_THREAD_LOCAL_TYPE(Pointer<TaskLocal::Storage>,
                                 tls_key::concurrency_fallback) Value;

public:
  static void set(TaskLocal::Storage *task) { Value.set(task); }
  static TaskLocal::Storage *get() { return Value.get(); }
};

/// Define the thread-locals.
SWIFT_THREAD_LOCAL_TYPE(Pointer<TaskLocal::Storage>,
                        tls_key::concurrency_fallback)
FallbackTaskLocalStorage::Value;

// ==== ABI --------------------------------------------------------------------

SWIFT_CC(swift)
static void swift_task_localValuePushImpl(const HeapObject *key,
                                              /* +1 */ OpaqueValue *value,
                                              const Metadata *valueType) {
  SWIFT_TASK_LOCAL_DEBUG_LOG(key, "push value: %p", value);
  if (AsyncTask *task = swift_task_getCurrent()) {
    task->localValuePush(key, value, valueType);
    return;
  }

  // no AsyncTask available so we must check the fallback
  TaskLocal::Storage *Local = nullptr;
  if (auto storage = FallbackTaskLocalStorage::get()) {
    Local = storage;
  } else {
    void *allocation = malloc(sizeof(TaskLocal::Storage));
    auto *freshStorage = new(allocation) TaskLocal::Storage();

    FallbackTaskLocalStorage::set(freshStorage);
    Local = freshStorage;
  }

  Local->pushValue(/*task=*/nullptr, key, value, valueType);
}

SWIFT_CC(swift)
static OpaqueValue* swift_task_localValueGetImpl(const HeapObject *key) {
  if (AsyncTask *task = swift_task_getCurrent()) {
    // we're in the context of a task and can use the task's storage
    auto value = task->localValueGet(key);
    SWIFT_TASK_LOCAL_DEBUG_LOG(key, "got value: %p", value);
    return value;
  }

  // no AsyncTask available so we must check the fallback
  if (auto Local = FallbackTaskLocalStorage::get()) {
    auto value = Local->getValue(/*task*/nullptr, key);
    SWIFT_TASK_LOCAL_DEBUG_LOG(key, "got value: %p", value);
    return value;
  }

  // no value found in task-local or fallback thread-local storage.
  SWIFT_TASK_LOCAL_DEBUG_LOG(key, "got no value: %d", 0);
  return nullptr;
}

SWIFT_CC(swift)
static void swift_task_localValuePopImpl() {
  if (AsyncTask *task = swift_task_getCurrent()) {
    task->localValuePop();
    return;
  }

  if (TaskLocal::Storage *Local = FallbackTaskLocalStorage::get()) {
    bool hasRemainingBindings = Local->popValue(nullptr);
    if (!hasRemainingBindings) {
      // We clean up eagerly, it may be that this non-swift-concurrency thread
      // never again will use task-locals, and as such we better remove the storage.
      FallbackTaskLocalStorage::set(nullptr);
      free(Local);
    }
    return;
  }

  assert(false && "Attempted to pop value but no task or thread-local storage available!");
}

SWIFT_CC(swift)
static void swift_task_localsCopyToImpl(AsyncTask *target) {
  if (auto Current = TaskLocal::Storage::getCurrent(swift_task_getCurrent())) {
    Current->copyTo(target);
  }
}

// =============================================================================
// ==== Initialization ---------------------------------------------------------

TaskLocal::Storage*
TaskLocal::Storage::getCurrent(AsyncTask *current) {
  if (current) {
    return &current->_private().Local;
  } else if (auto *storage = FallbackTaskLocalStorage::get()) {
    return storage;
  }

  return nullptr;
}

void TaskLocal::Storage::initializeLinkParent(AsyncTask* task,
                                              AsyncTask* parent) {
  assert(task && "task must not be null when copying values into it");
  assert(!head && "initial task local storage was already initialized");
  assert(parent && "parent must be provided to link to it");

  auto item = parent->_private().Local.head;

  // Don't create parent task marker if there are no values at all.
  if (!item)
    return;

  auto tail = TaskLocal::MarkerItem::createParentTaskMarker(task);
  head = tail;

  // Set of keys for which we already have copied to the new task.
  // We only ever need to copy the *first* encounter of any given key,
  // because it is the most "specific"/"recent" binding and any other binding
  // of a key does not matter for the target task as it will never be able to
  // observe it.
    std::set<const HeapObject *,
               std::less<const HeapObject *>,
               swift::cxx_allocator<const HeapObject *>> copied = {};

  // If we are a child task in a task group, it may happen that we are calling
  // addTask specifically in such shape:
  //
  //     $local.withValue(theValue) { addTask {} }
  //
  // If this is the case, we MUST copy `theValue` (and any other such directly
  // wrapping the addTask value bindings), because those values will be popped
  // when withValue returns - breaking our structured concurrency guarantees
  // that we rely on for the "link directly to parent's task local Item".
  //
  // Values set outside the task group are not subject to this problem, as
  // their structural lifetime guarantee is upheld by the group scope
  // out-living any addTask created tasks.
  //
  // TODO(concurrency): This can be optimized to copy only from the CURRENT
  // group, but we need to detect this, e.g. by more flags in the items made
  // from a group?
  while (item && item->getKind() == Item::Kind::ValueInTaskGroupBody) {
    auto valueItem = cast<ValueItem>(item);
    // we only have to copy an item if it is the most recent binding of a key.
    // i.e. if we've already seen an item for this key, we can skip it.
    if (copied.emplace(valueItem->key).second) {
      valueItem->copyTo(task);
    } else {
      SWIFT_TASK_LOCAL_DEBUG_LOG(
          valueItem->key,
          "skip copy, already copied most recent value, value was [%p]",
          valueItem->getStoragePtr());
    }

    item = item->getNext();
  }

  if (item && item->getKind() == Item::Kind::ParentTaskMarker) {
    // it has no values, and just points to its parent,
    // therefore skip also skip pointing to that parent and point
    // to whichever parent it was pointing to as well, it may be its
    // immediate parent, or some super-parent.
    item = item->getNext();
  }

  if (item && item->getKind() == Item::Kind::StopLookupMarker) {
    // Stop marker also could have been created inside task group body
    // But we don't need to copy it. Instead we can break the chain.
    item = nullptr;
  }

  // The next item is not the "risky one" so we can directly link to it,
  // as we would have within normal child task relationships. E.g. this is
  // a parent or next pointer to a "safe" (withValue { withTaskGroup { ... } })
  // binding, so we re-link our current head to point at this item.
  tail->setNext(item);
}

TaskLocal::MarkerItem *TaskLocal::MarkerItem::create(AsyncTask *task,
                                                     Item *next, Kind kind) {
  size_t amountToAllocate = sizeof(MarkerItem);
  void *allocation = _swift_task_alloc_specific(task, amountToAllocate);
  return new (allocation) MarkerItem(next, kind);
}

TaskLocal::ValueItem *TaskLocal::ValueItem::create(AsyncTask *task,
                                                   const HeapObject *key,
                                                   const Metadata *valueType,
                                                   bool inTaskGroupBody) {
  auto next = task ? task->_private().Local.head
                   : FallbackTaskLocalStorage::get()->head;

  size_t amountToAllocate = ValueItem::itemSize(valueType);
  void *allocation = task ? _swift_task_alloc_specific(task, amountToAllocate)
                          : malloc(amountToAllocate);
  return ::new (allocation) ValueItem(next, key, valueType, inTaskGroupBody);
}

void TaskLocal::ValueItem::copyTo(AsyncTask *target) {
  assert(target && "TaskLocal item attempt to copy to null target task!");

  auto item =
      ValueItem::create(target, key, valueType, /*inTaskGroupBody=*/false);
  valueType->vw_initializeWithCopy(item->getStoragePtr(), getStoragePtr());

  /// A `copyTo` may ONLY be invoked BEFORE the task is actually scheduled,
  /// so right now we can safely copy the value into the task without additional
  /// synchronization.
  target->_private().Local.head = item;
}

// =============================================================================
// ==== checks -----------------------------------------------------------------

/// UNUSED: This is effectively not used anymore by new runtimes because we will
/// defensively copy in this situation since Swift 6, rather than crash as a
/// means of defence.
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
      "            group.addTask { ... }\n"
      "        }\n"
      "    }\n"
      "\n"
      "And should be replaced by, either: setting the value for the entire group:\n"
      "\n"
      "    // bind task-local for all tasks spawned within the group\n"
      "    await <task-local>.withValue(1234) {\n"
      "        await withTaskGroup(...) { group in\n"
      "            group.addTask { ... }\n"
      "        }\n"
      "    }\n"
      "\n"
      "or, inside the specific task-group child task:\n"
      "\n"
      "    // bind-task-local for only specific child-task\n"
      "    await withTaskGroup(...) { group in\n"
      "        group.addTask {\n"
      "            await <task-local>.withValue(1234) {\n"
      "                ... \n"
      "            }\n"
      "        }\n"
      "\n"
      "        group.addTask { ... }\n"
      "    }\n",
      (int)fileLength, file,
      (int)line);

  if (_swift_shouldReportFatalErrorsToDebugger()) {
    RuntimeErrorDetails details = {
        .version = RuntimeErrorDetails::currentVersion,
        .errorType = "task-local-violation",
        .currentStackDescription = "Task-local bound in illegal context",
        .framesToSkip = 1,
        .memoryAddress = nullptr,
        .numExtraThreads = 0,
        .threads = nullptr,
        .numFixIts = 0,
        .fixIts = nullptr,
        .numNotes = 0,
        .notes = nullptr,
    };
    _swift_reportToDebugger(RuntimeErrorFlagFatal, message, &details);
  }

#if defined(_WIN32)
  #define STDERR_FILENO 2
  _write(STDERR_FILENO, message, strlen(message));
#else
  fputs(message, stderr);
  fflush(stderr);
#endif
#if SWIFT_STDLIB_HAS_ASL
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  asl_log(nullptr, nullptr, ASL_LEVEL_ERR, "%s", message);
#pragma clang diagnostic pop
#elif defined(__ANDROID__)
  __android_log_print(ANDROID_LOG_FATAL, "SwiftRuntime", "%s", message);
#endif

  free(message);
  abort();
}

// =============================================================================
// ==== destroy ----------------------------------------------------------------

bool TaskLocal::Item::destroy(AsyncTask *task) {
  bool stop = false;
  switch (getKind()) {
  case Kind::Value:
  case Kind::ValueInTaskGroupBody:
    cast<ValueItem>(this)->~ValueItem();
    break;
  case Kind::ParentTaskMarker:
    // we're done here; as we must not proceed into the parent owned values.
    // we do have to destroy the item pointing at the parent/edge itself though.
    stop = true;
    LLVM_FALLTHROUGH;
  case Kind::StopLookupMarker:
    cast<MarkerItem>(this)->~MarkerItem();
    break;
  }

  // if task is available, we must have used the task allocator to allocate this item,
  // so we must deallocate it using the same. Otherwise, we must have used malloc.
  if (task) _swift_task_dealloc_specific(task, this);
  else free(this);

  return stop;
}

void TaskLocal::Storage::destroy(AsyncTask *task) {
  auto item = head;
  head = nullptr;
  while (item) {
    TaskLocal::Item *next = item->getNext();
    if (item->destroy(task)) {
      return;
    }
    item = next;
  }
}

// =============================================================================
// ==== Task Local Storage: operations -----------------------------------------

void TaskLocal::Storage::pushValue(AsyncTask *task,
                                   const HeapObject *key,
                                   /* +1 */ OpaqueValue *value,
                                   const Metadata *valueType) {
  assert(value && "Task local value must not be nil");
  assert(swift_task_getCurrent() == task &&
         "must only be pushing task locals onto current task");

  // Why it matters to detect if we're pushing a value in a task group body:
  // We specifically need to prevent this pattern:
  //
  //    $number.withValue(0xBAADF00D) { // push
  //      group.addTask { ... }
  //    } // pop! BOOM!
  //
  // because the end of the withValue scope would pop the value,
  // and thus if the child task didn't copy the value, it'd refer to a bad
  // memory location at this point.
  bool inTaskGroupBody = swift_task_hasTaskGroupStatusRecord();

  TaskLocal::ValueItem *item =
      ValueItem::create(task, key, valueType, inTaskGroupBody);

  valueType->vw_initializeWithTake(item->getStoragePtr(), value);
  head = item;
  SWIFT_TASK_LOCAL_DEBUG_LOG(item->key, "Created link item:%p, in group body:%d",
                             item, inTaskGroupBody);
}

bool TaskLocal::Storage::popValue(AsyncTask *task) {
  assert(head && "attempted to pop value off empty task-local stack");
  auto valueItem = cast<ValueItem>(head);
  (void)valueItem;
  SWIFT_TASK_LOCAL_DEBUG_LOG(valueItem->key, "pop local item:%p, value:%p",
                             head, valueItem->getStoragePtr());

  auto old = head;
  head = head->getNext();
  old->destroy(task);

  /// if pointing at not-null next item, there are remaining bindings.
  return head != nullptr;
}

void TaskLocal::Storage::pushStopLookup(AsyncTask *task) {
  head = MarkerItem::createStopLookupMarker(task, head);
  SWIFT_TASK_LOCAL_DEBUG_LOG(nullptr, "push stop node item:%p", head);
}

void TaskLocal::Storage::popStopLookup(AsyncTask *task) {
  assert(head && "attempted to pop stop node off empty task-local stack");
  assert(head->getKind() == Item::Kind::StopLookupMarker &&
         "attempted to pop wrong node type");
  auto old = head;
  SWIFT_TASK_LOCAL_DEBUG_LOG(nullptr, "pop stop node item:%p", old);
  head = head->getNext();
  old->destroy(task);
}

OpaqueValue* TaskLocal::Storage::getValue(AsyncTask *task,
                                          const HeapObject *key) {
  assert(key && "TaskLocal key must not be null.");

  auto item = head;
  while (item) {
    if (auto valueItem = dyn_cast<ValueItem>(item)) {
      if (valueItem->key == key) {
        return valueItem->getStoragePtr();
      }
    } else if (item->getKind() == Item::Kind::StopLookupMarker) {
      break;
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
  std::set<const HeapObject *,
           std::less<const HeapObject *>,
           swift::cxx_allocator<const HeapObject *>> copied = {};

  auto item = head;
  while (item) {
    if (auto valueItem = dyn_cast<ValueItem>(item)) {
      // we only have to copy an item if it is the most recent binding of a key.
      // i.e. if we've already seen an item for this key, we can skip it.
      if (copied.emplace(valueItem->key).second) {
        valueItem->copyTo(target);
      } else {
        SWIFT_TASK_LOCAL_DEBUG_LOG(
            valueItem->key,
            "skip copy, already copied most recent value, value was [%p]",
            valueItem->getStoragePtr());
      }
    } else if (item->getKind() == Item::Kind::StopLookupMarker) {
      break;
    }
    item = item->getNext();
  }
}

TaskLocal::StopLookupScope::StopLookupScope() {
  task = swift_task_getCurrent();
  storage = Storage::getCurrent(task);
  if (storage && storage->isEmpty()) {
    storage = nullptr;
  }

  if (storage) {
    storage->pushStopLookup(task);
  }
}

TaskLocal::StopLookupScope::~StopLookupScope() {
  if (storage) {
    storage->popStopLookup(task);
  }
}

#define OVERRIDE_TASK_LOCAL COMPATIBILITY_OVERRIDE
#include "../CompatibilityOverride/CompatibilityOverrideIncludePath.h"
