//===--- TaskLocal.h - ABI of task local values -----------------*- C++ -*-===//
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
// Swift ABI describing task locals.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_TASKLOCAL_H
#define SWIFT_ABI_TASKLOCAL_H

#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {
class AsyncTask;
struct OpaqueValue;
struct SwiftError;
class TaskStatusRecord;
class TaskGroup;

// ==== Task Locals Values ---------------------------------------------------

class TaskLocal {
public:
  class Item {
  public:
    enum class Kind {
      /// Regular task local binding.
      Value = 0,

      /// Task local binding created inside the body of a `withTaskGroup`,
      /// and therefore we must either copy it, or crash when a child task is
      /// created
      /// using 'group.addTask' and it would refer to this task local.
      ///
      /// Items of this kind must be copied by a group child task for access
      /// safety reasons, as otherwise the pop would happen before the child
      /// task
      /// has completed.
      ValueInTaskGroupBody = 1,

      /// Artificial empty item that indicates end of items owned by the current
      /// task.
      /// The `getNext()` points at a item owned by another AsyncTask.
      ///
      /// Note that this may not necessarily be the same as the task's parent
      /// task -- we may point to a super-parent if we know / that the parent
      /// does not "contribute" any task local values. This is to speed up
      /// lookups by skipping empty parent tasks during get(), and explained
      /// in depth in `initializeLinkParent()`.
      ParentTaskMarker = 2,

      /// Marker item that indicates that all earlier items should be ignored.
      /// Allows to temporary disable all task local values in O(1).
      /// Is used to disable task-locals for fast path of isolated deinit.
      StopLookupMarker = 3,
    };

  private:
    llvm::PointerIntPair<Item *, 2, Kind> nextAndKind;

  protected:
    explicit Item(Item *next, Kind kind) : nextAndKind(next, kind) {}

  public:
    Kind getKind() const { return nextAndKind.getInt(); }

    Item *getNext() const { return nextAndKind.getPointer(); }
    void setNext(Item *next) { nextAndKind.setPointer(next); }

    bool destroy(AsyncTask *task);
  };

  class ValueItem : public Item {
    explicit ValueItem(Item *next, const HeapObject *key,
                       const Metadata *valueType, bool inTaskGroupBody)
        : Item(next,
               inTaskGroupBody ? Kind::ValueInTaskGroupBody : Kind::Value),
          key(key), valueType(valueType) {
      assert(key && valueType);
    }

  public:
    ~ValueItem() { valueType->vw_destroy(getStoragePtr()); }

    /// The type of the key with which this value is associated.
    const HeapObject *key;
    /// The type of the value stored by this item.
    const Metadata *valueType;

    // Trailing storage for the value itself. The storage will be
    // uninitialized or contain an instance of \c valueType.

    static ValueItem *create(AsyncTask *task, const HeapObject *key,
                             const Metadata *valueType, bool inTaskGroupBody);

    void copyTo(AsyncTask *task);

    bool isInTaskGroupBody() const {
      return getKind() == Kind::ValueInTaskGroupBody;
    }

    /// Retrieve a pointer to the storage of the value.
    OpaqueValue *getStoragePtr() {
      return reinterpret_cast<OpaqueValue *>(
        reinterpret_cast<char *>(this) + storageOffset(valueType));
    }

    /// Compute the offset of the storage from the base of the item.
    static size_t storageOffset(const Metadata *valueType) {
      size_t alignment = valueType->vw_alignment();
      return (sizeof(ValueItem) + alignment - 1) & ~(alignment - 1);
    }

    /// Determine the size of the item given a particular value type.
    static size_t itemSize(const Metadata *valueType) {
      size_t offset = storageOffset(valueType);
      offset += valueType->vw_size();
      return offset;
    }

    static bool classof(const Item *item) {
      return item->getKind() == Kind::Value ||
             item->getKind() == Kind::ValueInTaskGroupBody;
    }
  };

  class MarkerItem : public Item {
    MarkerItem(Item *next, Kind kind) : Item(next, kind) {}

    static MarkerItem *create(AsyncTask *task, Item *next, Kind kind);

  public:
    static MarkerItem *createParentTaskMarker(AsyncTask *task) {
      return create(task, nullptr, Kind::ParentTaskMarker);
    }
    static MarkerItem *createStopLookupMarker(AsyncTask *task, Item *next) {
      return create(task, next, Kind::StopLookupMarker);
    }

    static bool classof(const Item *item) {
      return item->getKind() == Kind::ParentTaskMarker ||
             item->getKind() == Kind::StopLookupMarker;
    }
  };

  class Storage {
    friend class TaskLocal::ValueItem;

  private:
    /// A stack (single-linked list) of task local values.
    ///
    /// Once task local values within this task are traversed, the list continues
    /// to the "next parent that contributes task local values," or if no such
    /// parent exists it terminates with null.
    ///
    /// If the TaskLocalValuesFragment was allocated, it is expected that this
    /// value should be NOT null; it either has own values, or at least one
    /// parent that has values. If this task does not have any values, the head
    /// pointer MAY immediately point at this task's parent task which has values.
    ///
    /// ### Concurrency
    /// Access to the head is only performed from the task itself, when it
    /// creates child tasks, the child during creation will inspect its parent's
    /// task local value stack head, and point to it. This is done on the calling
    /// task, and thus needs not to be synchronized. Subsequent traversal is
    /// performed by child tasks concurrently, however they use their own
    /// pointers/stack and can never mutate the parent's stack.
    ///
    /// The stack is only pushed/popped by the owning task, at the beginning and
    /// end a `body` block of `withLocal(_:boundTo:body:)` respectively.
    ///
    /// Correctness of the stack strongly relies on the guarantee that tasks
    /// never outline a scope in which they are created. Thanks to this, if
    /// tasks are created inside the `body` of `withLocal(_:,boundTo:body:)`
    /// all tasks created inside the `withLocal` body must complete before it
    /// returns, as such, any child tasks potentially accessing the value stack
    /// are guaranteed to be completed by the time we pop values off the stack
    /// (after the body has completed).
    TaskLocal::Item *head = nullptr;

  public:

    /// Get the "current" task local storage from either the passed in
    /// task, or fall back to the *thread* local stored storage.
    static Storage* getCurrent(AsyncTask *task);

    void initializeLinkParent(AsyncTask *task, AsyncTask *parent);

    bool isEmpty() const { return head == nullptr; }

    void pushValue(AsyncTask *task,
                   const HeapObject *key,
                   /* +1 */ OpaqueValue *value, const Metadata *valueType);

    OpaqueValue* getValue(AsyncTask *task, const HeapObject *key);

    /// Returns `true` of more bindings remain in this storage,
    /// and `false` if the just popped value was the last one and the storage
    /// can be safely disposed of.
    bool popValue(AsyncTask *task);

    void pushStopLookup(AsyncTask *task);
    void popStopLookup(AsyncTask *task);

    /// Copy all task-local bindings to the target task.
    ///
    /// The new bindings allocate their own items and can out-live the current task.
    ///
    /// ### Optimizations
    /// Only the most recent binding of a value is copied over, i.e. given
    /// a key bound to `A` and then `B`, only the `B` binding will be copied.
    /// This is safe and correct because the new task would never have a chance
    /// to observe the `A` value, because it semantically will never observe a
    /// "pop" of the `B` value - it was spawned from a scope where only B was observable.
    void copyTo(AsyncTask *target);

    /// Destroy and deallocate all items stored by this specific task.
    ///
    /// Items owned by a parent task are left untouched, since we do not own them.
    void destroy(AsyncTask *task);
  };

  class StopLookupScope {
    AsyncTask *task;
    Storage *storage;

  public:
    StopLookupScope();
    ~StopLookupScope();
  };
};

} // end namespace swift

#endif
