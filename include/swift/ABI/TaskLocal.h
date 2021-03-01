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
// Swift ABI describing tasks.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_TASKLOCAL_H
#define SWIFT_ABI_TASKLOCAL_H

#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h"

namespace swift {
class AsyncTask;
struct OpaqueValue;
struct SwiftError;
class TaskStatusRecord;
class TaskGroup;

// ==== Task Locals Values ---------------------------------------------------

class TaskLocal {
public:
  /// Type of the pointed at `next` task local item.
  enum class NextLinkType : uintptr_t {
    /// This task is known to be a "terminal" node in the lookup of task locals.
    /// In other words, even if it had a parent, the parent (and its parents)
    /// are known to not contain any any more task locals, and thus any further
    /// search beyond this task.
    IsTerminal = 0b00,
    /// The storage pointer points at the next TaskLocal::Item in this task.
    IsNext = 0b01,
    /// The storage pointer points at a item stored by another AsyncTask.
    ///
    /// Note that this may not necessarily be the same as the task's parent
    /// task -- we may point to a super-parent if we know / that the parent
    /// does not "contribute" any task local values. This is to speed up
    /// lookups by skipping empty parent tasks during get(), and explained
    /// in depth in `createParentLink`.
    IsParent = 0b11
  };

  /// Values must match `TaskLocalInheritance` declared in `TaskLocal.swift`.
  enum class TaskLocalInheritance : uint8_t {
    /// Default task local value behavior
    ///
    /// Values declared with a default-inherited key are accessible from:
    /// - the current task that has bound the value,
    /// - any child task of the current task (e.g. created by async let or groups)
    ///
    /// Such values are *not* carried through detached tasks.
    Default = 0,

    /// Special semantics which confine a task's local value to *only* the current
    /// task. In other words, they ave never inherited by any child task created
    /// by the current task.
    ///
    /// Values declared with a never-inherited key only accessible:
    /// - specifically from the current task itself
    ///
    /// Such values are *not* accessible from child tasks or detached tasks.
    Never   = 1
  };

  class Item {
  private:
    /// Mask used for the low status bits in a task local chain item.
    static const uintptr_t statusMask = 0x03;

    /// Pointer to the next task local item; be it in this task or in a parent.
    /// Low bits encode `NextLinkType`.
    /// Item *next = nullptr;
    uintptr_t next;

  public:
    /// The type of the key with which this value is associated.
    const Metadata *keyType;
    /// The type of the value stored by this item.
    const Metadata *valueType;

    // Trailing storage for the value itself. The storage will be
    // uninitialized or contain an instance of \c valueType.

  private:
    explicit Item()
      : next(0),
        keyType(nullptr),
        valueType(nullptr) {}

    explicit Item(const Metadata *keyType, const Metadata *valueType)
      : next(0),
        keyType(keyType),
        valueType(valueType) {}

  public:
    /// Item which does not by itself store any value, but only points
    /// to the nearest task-local-value containing parent's first task item.
    ///
    /// This item type is used to link to the appropriate parent task's item,
    /// when the current task itself does not have any task local values itself.
    ///
    /// When a task actually has its own task locals, it should rather point
    /// to the parent's *first* task-local item in its *last* item, extending
    /// the Item linked list into the appropriate parent.
    static Item *createParentLink(AsyncTask *task, AsyncTask *parent);

    static Item *createLink(AsyncTask *task,
                            const Metadata *keyType,
                            const Metadata *valueType);

    void destroy(AsyncTask *task);

    Item *getNext() {
      return reinterpret_cast<Item *>(next & ~statusMask);
    }

    NextLinkType getNextLinkType() {
      return static_cast<NextLinkType>(next & statusMask);
    }

    /// Item does not contain any actual value, and is only used to point at
    /// a specific parent item.
    bool isEmpty() {
      return !valueType;
    }

    /// Retrieve a pointer to the storage of the value.
    OpaqueValue *getStoragePtr() {
      return reinterpret_cast<OpaqueValue *>(
        reinterpret_cast<char *>(this) + storageOffset(valueType));
    }

    /// Compute the offset of the storage from the base of the item.
    static size_t storageOffset(const Metadata *valueType) {
      size_t offset = sizeof(Item);
      if (valueType) {
        size_t alignment = valueType->vw_alignment();
        return (offset + alignment - 1) & ~(alignment - 1);
      } else {
        return offset;
      }
    }

    /// Determine the size of the item given a particular value type.
    static size_t itemSize(const Metadata *valueType) {
      size_t offset = storageOffset(valueType);
      if (valueType) {
        offset += valueType->vw_size();
      }
      return offset;
    }
  };


  class Storage {
    friend class TaskLocal::Item;
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
    TaskLocal::Item *head;

  public:

    void initializeLinkParent(AsyncTask *task, AsyncTask *parent);

    void pushValue(AsyncTask *task,
                   const Metadata *keyType,
                   /* +1 */ OpaqueValue *value, const Metadata *valueType);

    OpaqueValue* getValue(AsyncTask *task,
                          const Metadata *keyType,
                          TaskLocalInheritance inheritance);

    void popValue(AsyncTask *task);

    /// Destroy and deallocate all items stored by this specific task.
    ///
    /// Items owned by a parent task are left untouched, since we do not own them.
    void destroy(AsyncTask *task);
  };
};

} // end namespace swift

#endif
