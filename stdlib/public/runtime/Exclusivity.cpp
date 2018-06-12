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
// Workaround: has_feature(cxx_thread_local) is wrong on two old Apple
// simulators. clang thinks thread_local works there, but it doesn't.
#if TARGET_OS_SIMULATOR && !TARGET_RT_64_BIT &&                      \
  ((TARGET_OS_IOS && __IPHONE_OS_VERSION_MIN_REQUIRED__ < 100000) || \
   (TARGET_OS_WATCH && __WATCHOS_OS_VERSION_MIN_REQUIRED__ < 30000))
// 32-bit iOS 9 simulator or 32-bit watchOS 2 simulator - use pthreads
# define SWIFT_EXCLUSIVITY_USE_THREADLOCAL 0
# define SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC 1
#elif __clang__ && !__has_feature(cxx_thread_local)
// clang without thread_local support - use pthreads
# define SWIFT_EXCLUSIVITY_USE_THREADLOCAL 0
# define SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC 1
#else
// Use thread_local
# define SWIFT_EXCLUSIVITY_USE_THREADLOCAL 1
# define SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC 0
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
  constexpr long maxReportedConflicts = 100;
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
           "Simultaneous accesses to 0x%" PRIxPTR ", but modification requires "
           "exclusive access",
           reinterpret_cast<uintptr_t>(pointer));
  fprintf(stderr, "%s.\n", message);

  char oldAccess[maxAccessDescriptionLength];
  snprintf(oldAccess, sizeof(oldAccess),
           "Previous access (a %s) started at", getAccessName(oldAction));
  fprintf(stderr, "%s ", oldAccess);
  if (oldPC) {
    dumpStackTraceEntry(0, oldPC, /*shortOutput=*/true);
    fprintf(stderr, " (0x%" PRIxPTR ").\n", reinterpret_cast<uintptr_t>(oldPC));
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
    .version = RuntimeErrorDetails::currentVersion,
    .errorType = "exclusivity-violation",
    .currentStackDescription = newAccess,
    .framesToSkip = framesToSkip,
    .memoryAddress = pointer,
    .numExtraThreads = 1,
    .threads = &secondaryThread
  };
  uintptr_t flags = RuntimeErrorFlagNone;
  if (!keepGoing)
    flags = RuntimeErrorFlagFatal;
  _swift_reportToDebugger(flags, message, &details);

  if (keepGoing) {
    return;
  }

  // 0 means no backtrace will be printed.
  fatalError(0, "Fatal access conflict detected.\n");
}

namespace {

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
    return reinterpret_cast<Access*>(NextAndAction & NextMask);
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
class AccessSet {
  Access *Head = nullptr;
public:
  constexpr AccessSet() {}

  bool insert(Access *access, void *pc, void *pointer, ExclusivityFlags flags) {
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
    if (!isTracking(flags))
      return false;

    // Insert to the front of the array so that remove tends to find it faster.
    access->initialize(pc, pointer, Head, action);
    Head = access;
    return true;
  }

  void remove(Access *access) {
    auto cur = Head;
    // Fast path: stack discipline.
    if (cur == access) {
      Head = cur->getNext();
      return;
    }

    Access *last = cur;
    for (cur = cur->getNext(); cur != nullptr;
         last = cur, cur = cur->getNext()) {
      assert(last->getNext() == cur);
      if (cur == access) {
        last->setNext(cur->getNext());
        return;
      }
    }

    swift_runtime_unreachable("access not found in set");
  }

#ifndef NDEBUG
  /// Only available with asserts. Intended to be used with
  /// swift_dumpTrackedAccess().
  void forEach(std::function<void (Access *)> action) {
    for (auto *iter = Head; iter != nullptr; iter = iter->getNext()) {
      action(iter);
    }
  }
#endif
};

} // end anonymous namespace

// Each of these cases should define a function with this prototype:
//   AccessSets &getAllSets();

#if SWIFT_EXCLUSIVITY_USE_THREADLOCAL
// Use direct language support for thread-locals.

static_assert(LLVM_ENABLE_THREADS, "LLVM_THREAD_LOCAL will use a global?");
static LLVM_THREAD_LOCAL AccessSet ExclusivityAccessSet;

static AccessSet &getAccessSet() {
  return ExclusivityAccessSet;
}

#elif SWIFT_EXCLUSIVITY_USE_PTHREAD_SPECIFIC
// Use pthread_getspecific.

static pthread_key_t createAccessSetPthreadKey() {
  pthread_key_t key;
  int result = pthread_key_create(&key, [](void *pointer) {
    delete static_cast<AccessSet*>(pointer);
  });

  if (result != 0) {
    fatalError(0, "couldn't create pthread key for exclusivity: %s\n",
               strerror(result));
  }
  return key;
}

static AccessSet &getAccessSet() {
  static pthread_key_t key = createAccessSetPthreadKey();

  AccessSet *set = static_cast<AccessSet*>(pthread_getspecific(key));
  if (!set) {
    set = new AccessSet();
    pthread_setspecific(key, set);
  }
  return *set;
}

/** An access set accessed via pthread_get_specific. *************************/
#else
#error No implementation chosen for exclusivity!
#endif

/// Begin tracking a dynamic access.
///
/// This may cause a runtime failure if an incompatible access is
/// already underway.
void swift::swift_beginAccess(void *pointer, ValueBuffer *buffer,
                              ExclusivityFlags flags, void *pc) {
  assert(pointer && "beginning an access on a null pointer?");

  Access *access = reinterpret_cast<Access*>(buffer);

  // If exclusivity checking is disabled, record in the access buffer that we
  // didn't track anything. pc is currently undefined in this case.
  if (_swift_disableExclusivityChecking) {
    access->Pointer = nullptr;
    return;
  }

  // If the provided `pc` is null, then the runtime may override it for
  // diagnostics.
  if (!pc)
    pc = get_return_address();

  if (!getAccessSet().insert(access, pc, pointer, flags))
    access->Pointer = nullptr;
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

  getAccessSet().remove(access);
}

#ifndef NDEBUG

// Dump the accesses that are currently being tracked by the runtime.
//
// This is only intended to be used in the debugger.
void swift::swift_dumpTrackedAccesses() {
  getAccessSet().forEach([](Access *a) {
      fprintf(stderr, "Access. Pointer: %p. PC: %p. AccessAction: %s\n",
              a->Pointer, a->PC, getAccessName(a->getAccessAction()));
  });
}

#endif
