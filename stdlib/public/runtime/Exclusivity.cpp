//===--- Exclusivity.cpp - Exclusivity tracking ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements the runtime support for dynamically tracking exclusivity.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/Metadata.h"
#include <memory>

// Pick an implementation strategy.
#ifndef SWIFT_EXCLUSIVITY_USE_THREADLOCAL

// If we're using Clang, and Clang claims not to support thread_local,
// it must be because we're on a platform that doesn't support it.
// Use pthreads.
#if __clang__ && !__has_feature(cxx_thread_local)
#define SWIFT_EXCLUSIVITY_USE_THREADLOCAL 0
#define SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC 1
#else
#define SWIFT_EXCLUSIVITY_USE_THREADLOCAL 1
#define SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC 0
#endif

#endif

#if SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC
#include <pthread.h>
#include <stdio.h>
#endif

using namespace swift;

bool swift::_swift_disableExclusivityChecking = false;

static const char *getAccessName(ExclusivityFlags flags) {
  switch (flags) {
  case ExclusivityFlags::Read: return "read";
  case ExclusivityFlags::Modify: return "modify";
  }
  return "unknown";
}

namespace {

/// A single access that we're tracking.
struct Access {
  void *Pointer;
  uintptr_t HereAndAction;
  Access *Next;

  enum : uintptr_t {
    ActionMask = (uintptr_t)ExclusivityFlags::ActionMask,
    HereMask = ~ActionMask
  };

  Access **getHere() const {
    return reinterpret_cast<Access**>(HereAndAction & HereMask);
  }

  void setHere(Access **newHere) {
    HereAndAction = reinterpret_cast<uintptr_t>(newHere) | uintptr_t(getAccessAction());
  }

  ExclusivityFlags getAccessAction() const {
    return ExclusivityFlags(HereAndAction & ActionMask);
  }

  void initialize(void *pointer, Access **here, ExclusivityFlags action) {
    assert(*here == nullptr && "not inserting to end of list");
    Pointer = pointer;
    HereAndAction = reinterpret_cast<uintptr_t>(here) | uintptr_t(action);
    Next = nullptr;
  }
};

static_assert(sizeof(Access) <= sizeof(ValueBuffer) &&
              alignof(Access) <= alignof(ValueBuffer),
              "Access doesn't fit in a value buffer!");

/// A set of accesses that we're tracking.  Just a singly-linked list.
class AccessSet {
  Access *Head = nullptr;
public:
  constexpr AccessSet() {}

  void insert(Access *access, void *pointer, ExclusivityFlags flags) {
    auto action = getAccessAction(flags);

    Access **curP = &Head;
    for (Access *cur = *curP; cur != nullptr; curP = &cur->Next, cur = *curP) {
      // Ignore accesses to different values.
      if (cur->Pointer != pointer)
        continue;

      // If both accesses are reads, it's not a conflict.
      if (action == ExclusivityFlags::Read &&
          action == cur->getAccessAction())
        continue;

      // Otherwise, it's a conflict.
      // TODO: try to recover source-location information from the return
      // address.
      fatalError(0, "%s/%s access conflict detected on address %p, aborting\n",
                 getAccessName(action), getAccessName(cur->getAccessAction()),
                 pointer);
    }

    access->initialize(pointer, curP, action);
    *curP = access;
  }

  static void remove(Access *access) {
    Access **here = access->getHere();
    *here = access->Next;
    if (access->Next != nullptr)
      access->Next->setHere(here);
  }
};

/// A set of independent access sets.  This is not designed to put
/// the access sets on different cache lines, so it's fine for
/// thread-local sets and probably not fine for concurrent sets.
class AccessSets {
  enum { NumAccessSets = 8 };
  AccessSet Sets[NumAccessSets];

public:
  constexpr AccessSets() = default;
  AccessSets(const AccessSets &) = delete;
  AccessSets &operator=(const AccessSets &) = delete;

  AccessSet &get(void *pointer) {
    size_t index = std::hash<void*>()(pointer) % NumAccessSets;
    return Sets[index];
  }
};

} // end anonymous namespace

// Each of these cases should define a function with this prototype:
//   AccessSets &getAllSets();

#if SWIFT_EXCLUSIVITY_USE_THREADLOCAL
// Use direct language support for thread-locals.

static_assert(LLVM_ENABLE_THREADS, "LLVM_THREAD_LOCAL will use a global?");
static LLVM_THREAD_LOCAL AccessSets ExclusivityAccessSets;

static AccessSets &getAllSets() {
  return ExclusivityAccessSets;
}

#elif SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC
// Use pthread_getspecific.

static pthread_key_t createAccessSetPthreadKey() {
  pthread_key_t key;
  int result = pthread_key_create(&key, [](void *pointer) {
    delete static_cast<AccessSets*>(pointer);
  });

  if (result != 0) {
    fatalError(0, "couldn't create pthread key for exclusivity: %s\n",
               strerror(result));
  }
  return key;
}

static AccessSets &getAllSets() {
  static pthread_key_t key = createAccessSetPthreadKey();

  AccessSets *sets = static_cast<AccessSets*>(pthread_getspecific(key));
  if (!sets) {
    sets = new AccessSets();
    pthread_setspecific(key, sets);
  }
  return *sets;
}

/** An access set accessed via pthread_get_specific. *************************/
#else
#error No implementation chosen for exclusivity!
#endif

/// Return the right access set for the given pointer.
static AccessSet &getAccessSet(void *pointer) {
  return getAllSets().get(pointer);
}

/// Begin tracking a dynamic access.
///
/// This may cause a runtime failure if an incompatible access is
/// already underway.
void swift::swift_beginAccess(void *pointer, ValueBuffer *buffer,
                              ExclusivityFlags flags) {
  assert(pointer && "beginning an access on a null pointer?");

  Access *access = reinterpret_cast<Access*>(buffer);

  // If exclusivity checking is disabled, record in the access
  // buffer that we didn't track anything.
  if (_swift_disableExclusivityChecking) {
    access->Pointer = nullptr;
    return;
  }

  getAccessSet(pointer).insert(access, pointer, flags);
}

/// End tracking a dynamic access.
void swift::swift_endAccess(ValueBuffer *buffer) {
  Access *access = reinterpret_cast<Access*>(buffer);
  auto pointer = access->Pointer;

  // If the pointer in the access is null, we must've declined
  // to track it because exclusivity tracking was disabled.
  if (!pointer) {
    return;
  }

  AccessSet::remove(access);
}
