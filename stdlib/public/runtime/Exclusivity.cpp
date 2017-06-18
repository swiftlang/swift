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
#include <stdio.h>

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
#endif

// Pick a return-address strategy
#if __GNUC__
#define get_return_address() __builtin_return_address(0)
#elif _MSC_VER
#include <intrin.h>
#define get_return_address() _ReturnAddress()
#else
#error missing implementation for get_return_address
#define get_return_address() ((void*) 0)
#endif

using namespace swift;

bool swift::_swift_disableExclusivityChecking = false;

static const char *getAccessName(ExclusivityFlags flags) {
  switch (flags) {
  case ExclusivityFlags::Read: return "read";
  case ExclusivityFlags::Modify: return "modification";
  default: return "unknown";
  }
}

LLVM_ATTRIBUTE_ALWAYS_INLINE
static void reportExclusivityConflict(ExclusivityFlags oldAction, void *oldPC,
                                      ExclusivityFlags newFlags, void *newPC,
                                      void *pointer) {
  static std::atomic<long> reportedConflicts{0};
  constexpr unsigned maxReportedConflicts = 100;
  // Don't report more that 100 conflicts. Hopefully, this will improve
  // performance in case there are conflicts inside a tight loop.
  if (reportedConflicts.fetch_add(1, std::memory_order_relaxed) >=
      maxReportedConflicts) {
    return;
  }

  constexpr unsigned maxMessageLength = 100;
  constexpr unsigned maxAccessDescriptionLength = 50;
  char message[maxMessageLength];
  snprintf(message, sizeof(message),
           "Simultaneous accesses to 0x%lx, but modification requires "
           "exclusive access",
           (uintptr_t)pointer);
  fprintf(stderr, "%s.\n", message);

  char oldAccess[maxAccessDescriptionLength];
  snprintf(oldAccess, sizeof(oldAccess),
           "Previous access (a %s) started at", getAccessName(oldAction));
  fprintf(stderr, "%s ", oldAccess);
  if (oldPC) {
    dumpStackTraceEntry(0, oldPC, /*shortOutput=*/true);
    fprintf(stderr, " (0x%lx).\n", (uintptr_t)oldPC);
  } else {
    fprintf(stderr, "<unknown>.\n");
  }

  char newAccess[maxAccessDescriptionLength];
  snprintf(newAccess, sizeof(newAccess), "Current access (a %s) started at",
           getAccessName(getAccessAction(newFlags)));
  fprintf(stderr, "%s:\n", newAccess);
  // The top frame is in swift_beginAccess, don't print it.
  constexpr unsigned framesToSkip = 1;
  printCurrentBacktrace(framesToSkip);

  bool keepGoing = isWarningOnly(newFlags);

  RuntimeErrorDetails::Thread secondaryThread = {
    .description = oldAccess,
    .numFrames = 1,
    .frames = &oldPC
  };
  RuntimeErrorDetails details = {
    .version = 1,
    .errorType = "exclusivity-violation",
    .currentStackDescription = newAccess,
    .framesToSkip = framesToSkip,
    .memoryAddress = pointer,
    .numExtraThreads = 1,
    .threads = &secondaryThread
  };
  reportToDebugger(!keepGoing, message, &details);

  if (keepGoing) {
    return;
  }

  // 0 means no backtrace will be printed.
  fatalError(0, "Fatal access conflict detected.\n");
}

namespace {

/// A single access that we're tracking.
struct Access {
  void *Pointer;
  void *PC;
  uintptr_t NextAndAction;

  enum : uintptr_t {
    ActionMask = (uintptr_t)ExclusivityFlags::ActionMask,
    NextMask = ~ActionMask
  };

  Access *getNext() const {
    return reinterpret_cast<Access*>(NextAndAction & NextMask);
  }

  void setNext(Access *next) {
    NextAndAction =
      reinterpret_cast<uintptr_t>(next) | (NextAndAction & NextMask);
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
class AccessSet {
  Access *Head = nullptr;
public:
  constexpr AccessSet() {}

  void insert(Access *access, void *pc, void *pointer, ExclusivityFlags flags) {
    auto action = getAccessAction(flags);

    for (Access *cur = Head; cur != nullptr; cur = cur->getNext()) {
      // Ignore accesses to different values.
      if (cur->Pointer != pointer)
        continue;

      // If both accesses are reads, it's not a conflict.
      if (action == ExclusivityFlags::Read &&
          action == cur->getAccessAction())
        continue;

      // Otherwise, it's a conflict.
      reportExclusivityConflict(cur->getAccessAction(), cur->PC,
                                flags, pc, pointer);

      // If we're only warning, don't report multiple conflicts.
      break;
    }

    // Insert to the front of the array so that remove tends to find it faster.
    access->initialize(pc, pointer, Head, action);
    Head = access;
  }

  void remove(Access *access) {
    auto cur = Head;
    // Fast path: stack discipline.
    if (cur == access) {
      Head = cur->getNext();
      return;
    }

    for (Access *last = cur; cur != nullptr; last = cur, cur = cur->getNext()) {
      if (last == access) {
        last->setNext(cur->getNext());
        return;
      }
    }

    swift_runtime_unreachable("access not found in set");
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
                              ExclusivityFlags flags, void *pc) {
  assert(pointer && "beginning an access on a null pointer?");

  Access *access = reinterpret_cast<Access*>(buffer);

  // If exclusivity checking is disabled, record in the access
  // buffer that we didn't track anything.
  if (_swift_disableExclusivityChecking) {
    access->Pointer = nullptr;
    return;
  }

  if (!pc) pc = get_return_address();

  getAccessSet(pointer).insert(access, pc, pointer, flags);
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

  getAccessSet(pointer).remove(access);
}
