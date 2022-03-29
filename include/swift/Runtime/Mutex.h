//===--- Mutex.h - Mutex ----------------------------------------*- C++ -*-===//
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
// Mutex and Scoped lock abstractions for use in Swift runtime.
//
// We intentionally do not provide a condition-variable abstraction.
// Traditional condition-variable interfaces are subject to unavoidable
// priority inversions, as well as making poor use of threads.
// Prefer AtomicWaitQueue.
//
// We also intentionally avoid read/write locks.  It's difficult to implement a
// performant and fair read/write lock, and indeed many common implementations
// rely on condition variables, which, again, are subject to unavoidable
// priority inversions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_H
#define SWIFT_RUNTIME_MUTEX_H

#include <type_traits>
#include <utility>

#if __has_include(<unistd.h>)
#include <unistd.h>
#endif

#if SWIFT_STDLIB_THREADING_NONE
#include "swift/Runtime/MutexSingleThreaded.h"
#elif SWIFT_STDLIB_THREADING_PTHREADS || SWIFT_STDLIB_THREADING_DARWIN
#include "swift/Runtime/MutexPThread.h"
#elif SWIFT_STDLIB_THREADING_WIN32
#include "swift/Runtime/MutexWin32.h"
#elif SWIFT_STDLIB_THREADING_C11
#include "swift/Runtime/MutexC11.h"
#else
#error "Implement equivalent of MutexPThread.h/cpp for your platform."
#endif

namespace swift {

/// Compile time adjusted stack based object that locks/unlocks the supplied
/// Mutex type. Use the provided typedefs instead of this directly.
template <typename T, bool Inverted> class ScopedLockT {
  ScopedLockT() = delete;
  ScopedLockT(const ScopedLockT &) = delete;
  ScopedLockT &operator=(const ScopedLockT &) = delete;
  ScopedLockT(ScopedLockT &&) = delete;
  ScopedLockT &operator=(ScopedLockT &&) = delete;

public:
  explicit ScopedLockT(T &l) : Lock(l) {
    if (Inverted) {
      Lock.unlock();
    } else {
      Lock.lock();
    }
  }

  ~ScopedLockT() {
    if (Inverted) {
      Lock.lock();
    } else {
      Lock.unlock();
    }
  }

private:
  T &Lock;
};

/// A Mutex object that supports `BasicLockable` and `Lockable` C++ concepts.
/// See http://en.cppreference.com/w/cpp/concept/BasicLockable
/// See http://en.cppreference.com/w/cpp/concept/Lockable
///
/// This is NOT a recursive mutex.
class Mutex {

  Mutex(const Mutex &) = delete;
  Mutex &operator=(const Mutex &) = delete;
  Mutex(Mutex &&) = delete;
  Mutex &operator=(Mutex &&) = delete;

public:
  /// Constructs a non-recursive mutex.
  ///
  /// If `checked` is true the mutex will attempt to check for misuse and
  /// fatalError when detected. If `checked` is false (the default) the
  /// mutex will make little to no effort to check for misuse (more efficient).
  explicit Mutex(bool checked = false) {
    MutexPlatformHelper::init(Handle, checked);
  }
  ~Mutex() { MutexPlatformHelper::destroy(Handle); }

  /// The lock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread until exclusive ownership of the mutex
  ///   can be obtained.
  /// - Prior m.unlock() operations on the same mutex synchronize-with
  ///   this lock operation.
  /// - The behavior is undefined if the calling thread already owns
  ///   the mutex (likely a deadlock).
  /// - Does not throw exceptions but will halt on error (fatalError).
  void lock() { MutexPlatformHelper::lock(Handle); }

  /// The unlock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the mutex and
  ///   synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the mutex.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void unlock() { MutexPlatformHelper::unlock(Handle); }

  /// The try_lock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Attempts to obtain exclusive ownership of the mutex for the calling
  ///   thread without blocking. If ownership is not obtained, returns
  ///   immediately. The function is allowed to spuriously fail and return
  ///   even if the mutex is not currently owned by another thread.
  /// - If try_lock() succeeds, prior unlock() operations on the same object
  ///   synchronize-with this operation. lock() does not synchronize with a
  ///   failed try_lock()
  /// - The behavior is undefined if the calling thread already owns
  ///   the mutex (likely a deadlock)?
  /// - Does not throw exceptions but will halt on error (fatalError).
  bool try_lock() { return MutexPlatformHelper::try_lock(Handle); }

  /// Acquires lock before calling the supplied critical section and releases
  /// lock on return from critical section.
  ///
  /// This call can block while waiting for the lock to become available.
  ///
  /// For example the following mutates value while holding the mutex lock.
  ///
  /// ```
  ///   mutex.lock([&value] { value++; });
  /// ```
  ///
  /// Precondition: Mutex not held by this thread, undefined otherwise.
  template <typename CriticalSection>
  auto withLock(CriticalSection &&criticalSection)
      -> decltype(std::forward<CriticalSection>(criticalSection)()) {
    ScopedLock guard(*this);
    return std::forward<CriticalSection>(criticalSection)();
  }

  /// A stack based object that locks the supplied mutex on construction
  /// and unlocks it on destruction.
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  typedef ScopedLockT<Mutex, false> ScopedLock;

  /// A stack based object that unlocks the supplied mutex on construction
  /// and relocks it on destruction.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  typedef ScopedLockT<Mutex, true> ScopedUnlock;

private:
  MutexHandle Handle;
};

/// A static allocation variant of Mutex.
///
/// Use Mutex instead unless you need static allocation.
class StaticMutex {

  StaticMutex(const StaticMutex &) = delete;
  StaticMutex &operator=(const StaticMutex &) = delete;
  StaticMutex(StaticMutex &&) = delete;
  StaticMutex &operator=(StaticMutex &&) = delete;

public:
#if SWIFT_MUTEX_SUPPORTS_CONSTEXPR
  constexpr
#endif
  StaticMutex()
      : Handle(MutexPlatformHelper::staticInit()) {
  }

  /// See Mutex::lock
  void lock() { MutexPlatformHelper::lock(Handle); }

  /// See Mutex::unlock
  void unlock() { MutexPlatformHelper::unlock(Handle); }

  /// See Mutex::try_lock
  bool try_lock() { return MutexPlatformHelper::try_lock(Handle); }

  /// See Mutex::lock
  template <typename CriticalSection>
  auto withLock(CriticalSection &&criticalSection)
      -> decltype(std::forward<CriticalSection>(criticalSection)()) {
    ScopedLock guard(*this);
    return std::forward<CriticalSection>(criticalSection)();
  }

  /// A stack based object that locks the supplied mutex on construction
  /// and unlocks it on destruction.
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  typedef ScopedLockT<StaticMutex, false> ScopedLock;

  /// A stack based object that unlocks the supplied mutex on construction
  /// and relocks it on destruction.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  typedef ScopedLockT<StaticMutex, true> ScopedUnlock;

private:
  MutexHandle Handle;
};

/// A Mutex object that supports `BasicLockable` C++ concepts. It is
/// considered
/// unsafe to use because it doesn't do any error checking. It is only for
/// use in pathways that deal with reporting fatalErrors to avoid the
/// potential
/// for recursive fatalErrors that could happen if you used Mutex.
///
/// Always use Mutex, unless in the above mentioned error pathway situation.
class StaticUnsafeMutex {

  StaticUnsafeMutex(const StaticUnsafeMutex &) = delete;
  StaticUnsafeMutex &operator=(const StaticUnsafeMutex &) = delete;
  StaticUnsafeMutex(StaticUnsafeMutex &&) = delete;
  StaticUnsafeMutex &operator=(StaticUnsafeMutex &&) = delete;

public:
#if SWIFT_MUTEX_SUPPORTS_CONSTEXPR
  constexpr
#endif
  StaticUnsafeMutex()
      : Handle(MutexPlatformHelper::staticInit()) {
  }

  /// The lock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread until exclusive ownership of the mutex
  ///   can be obtained.
  /// - Prior m.unlock() operations on the same mutex synchronize-with
  ///   this lock operation.
  /// - The behavior is undefined if the calling thread already owns
  ///   the mutex (likely a deadlock).
  /// - Ignores errors that may happen, undefined when an error happens.
  void lock() { MutexPlatformHelper::unsafeLock(Handle); }

  /// The unlock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the mutex and
  ///   synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the mutex.
  /// - Ignores errors that may happen, undefined when an error happens.
  void unlock() { MutexPlatformHelper::unsafeUnlock(Handle); }

  template <typename CriticalSection>
  auto withLock(CriticalSection &&criticalSection)
      -> decltype(std::forward<CriticalSection>(criticalSection)()) {
    ScopedLock guard(*this);
    return std::forward<CriticalSection>(criticalSection)();
  }

  typedef ScopedLockT<StaticUnsafeMutex, false> ScopedLock;
  typedef ScopedLockT<StaticUnsafeMutex, true> ScopedUnlock;

private:
  MutexHandle Handle;
};

/// An indirect variant of a Mutex. This allocates the mutex on the heap, for
/// places where having the mutex inline takes up too much space. Used for
/// SmallMutex on platforms where Mutex is large.
class IndirectMutex {
  IndirectMutex(const IndirectMutex &) = delete;
  IndirectMutex &operator=(const IndirectMutex &) = delete;
  IndirectMutex(IndirectMutex &&) = delete;
  IndirectMutex &operator=(IndirectMutex &&) = delete;

public:
  explicit IndirectMutex(bool checked = false) { Ptr = new Mutex(checked); }
  ~IndirectMutex() { delete Ptr; }

  void lock() { Ptr->lock(); }

  void unlock() { Ptr->unlock(); }

  bool try_lock() { return Ptr->try_lock(); }

  template <typename CriticalSection>
  auto withLock(CriticalSection &&criticalSection)
      -> decltype(criticalSection()) {
    return Ptr->withLock(std::forward<CriticalSection>(criticalSection));
  }

  /// A stack based object that locks the supplied mutex on construction
  /// and unlocks it on destruction.
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  typedef ScopedLockT<IndirectMutex, false> ScopedLock;

  /// A stack based object that unlocks the supplied mutex on construction
  /// and relocks it on destruction.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  typedef ScopedLockT<IndirectMutex, true> ScopedUnlock;

private:
  Mutex *Ptr;
};

/// A "small" mutex, which is pointer sized or smaller, for places where the
/// mutex is stored inline with limited storage space. This uses a normal Mutex
/// when that is small, and otherwise uses IndirectMutex.
using SmallMutex =
    std::conditional_t<sizeof(Mutex) <= sizeof(void *), Mutex, IndirectMutex>;

// Enforce literal requirements for static variants.
#if SWIFT_MUTEX_SUPPORTS_CONSTEXPR
static_assert(std::is_literal_type<StaticMutex>::value,
              "StaticMutex must be literal type");
static_assert(std::is_literal_type<StaticUnsafeMutex>::value,
              "StaticUnsafeMutex must be literal type");
#else
// Your platform doesn't currently support statically allocated Mutex
// you will possibly see global-constructors warnings
#endif

#if SWIFT_CONDITION_SUPPORTS_CONSTEXPR
static_assert(std::is_literal_type<StaticConditionVariable>::value,
              "StaticConditionVariable must be literal type");
#else
// Your platform doesn't currently support statically allocated ConditionVar
// you will possibly see global-constructors warnings
#endif

}

#endif
