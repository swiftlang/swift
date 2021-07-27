//===--- ExclusivityPrivate.h ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_EXCLUSIVITYPRIVATE_H
#define SWIFT_RUNTIME_EXCLUSIVITYPRIVATE_H

#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/Metadata.h"
#include <cinttypes>

namespace swift {
namespace runtime {

/// A single access that we're tracking.
///
/// The following inputs are accepted by the begin_access runtime entry
/// point. This table show the action performed by the current runtime to
/// convert those inputs into stored fields in the Access scratch buffer.
///
/// Pointer | Runtime     | Access | PC    | Reported| Access
/// Argument| Behavior    | Pointer| Arg   | PC      | PC
/// -------- ------------- -------- ------- --------- ----------
/// null    | [trap or missing enforcement]
/// nonnull | [nontracked]| null   | null  | caller  | [discard]
/// nonnull | [nontracked]| null   | valid | <same>  | [discard]
/// nonnull | [tracked]   | <same> | null  | caller  | caller
/// nonnull | [tracked]   | <same> | valid | <same>  | <same>
///
/// [nontracked] means that the Access scratch buffer will not be added to the
/// runtime's list of tracked accesses. However, it may be passed to a
/// subsequent call to end_unpaired_access. The null Pointer field then
/// identifies the Access record as nontracked.
///
/// The runtime owns the contents of the scratch buffer, which is allocated by
/// the compiler but otherwise opaque. The runtime may later reuse the Pointer
/// or PC fields or any spare bits for additional flags, and/or a pointer to
/// out-of-line storage.
struct Access {
  void *Pointer;
  void *PC;
  uintptr_t NextAndAction;

  enum : uintptr_t {
    ActionMask = (uintptr_t)ExclusivityFlags::ActionMask,
    NextMask = ~ActionMask
  };

  Access *getNext() const {
    return reinterpret_cast<Access *>(NextAndAction & NextMask);
  }

  void setNext(Access *next) {
    NextAndAction =
        reinterpret_cast<uintptr_t>(next) | (NextAndAction & ActionMask);
  }

  ExclusivityFlags getAccessAction() const {
    return ExclusivityFlags(NextAndAction & ActionMask);
  }

  void initialize(void *pc, void *pointer, Access *next,
                  ExclusivityFlags action) {
    Pointer = pointer;
    PC = pc;
    NextAndAction = reinterpret_cast<uintptr_t>(next) | uintptr_t(action);
  }
};

static_assert(sizeof(Access) <= sizeof(ValueBuffer) &&
                  alignof(Access) <= alignof(ValueBuffer),
              "Access doesn't fit in a value buffer!");

/// A set of accesses that we're tracking.  Just a singly-linked list.
///
/// NOTE: Please keep all implementations of methods of AccessSet within
/// Exclusivity.cpp. We want this to ensure that when compiled in the runtime
/// directly, the definitions of these methods are immediately available in that
/// file for inlining.
class AccessSet {
  Access *Head = nullptr;

public:
  constexpr AccessSet() {}
  constexpr AccessSet(Access *Head) : Head(Head) {}

  constexpr operator bool() const { return bool(Head); }
  constexpr Access *getHead() const { return Head; }
  void setHead(Access *newHead) { Head = newHead; }
  constexpr bool isHead(Access *access) const { return Head == access; }

  bool insert(Access *access, void *pc, void *pointer, ExclusivityFlags flags);
  void remove(Access *access);

  /// Return the parent access of \p childAccess in the list.
  Access *findParentAccess(Access *childAccess) const {
    auto cur = Head;
    Access *last = cur;
    for (cur = cur->getNext(); cur != nullptr;
         last = cur, cur = cur->getNext()) {
      assert(last->getNext() == cur);
      if (cur == childAccess) {
        return last;
      }
    }
    return nullptr;
  }

  Access *getTail() const {
    auto cur = Head;
    if (!cur)
      return nullptr;

    while (auto *next = cur->getNext()) {
      cur = next;
    }
    assert(cur != nullptr);
    return cur;
  }

#ifndef NDEBUG
  /// Only available with asserts. Intended to be used with
  /// swift_dumpTrackedAccess().
  void forEach(std::function<void(Access *)> action);
#endif
};

} // namespace runtime
} // namespace swift

#endif
