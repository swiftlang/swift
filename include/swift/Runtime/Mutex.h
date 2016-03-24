//===--- Mutex.h - Lockables ------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex, Condition, and Scoped lock abstactions for use in Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_H
#define SWIFT_RUNTIME_MUTEX_H

#if (defined(__APPLE__) || defined(__linux__) || defined(__CYGWIN__))
#define SWIFT_RUNTIME_MUTEX_HAVE_PHTREADS
#else
#error "Must implement the following if your platform doesn't support phtreads."
#endif

#ifdef SWIFT_RUNTIME_MUTEX_HAVE_PHTREADS
#include <pthread.h>
#endif

namespace swift {

/// A Condition that works with Mutex to allow – as an example – multi-threaded
/// producers and consumers to signal each other in a safe way.
class Condition {
  friend class MutexImpl;

public:
  explicit Condition();
  ~Condition();

  Condition(const Condition &) = delete;
  Condition &operator=(const Condition &) = delete;
  Condition(Condition &&) = delete;
  Condition &operator=(Condition &&) = delete;

public:
  /// Notifies one waiter (if any exists) that the condition has been met.
  ///
  /// Note: To avoid missed notification it is best hold the related mutex
  //        lock when calling notifyOne.
  void notifyOne();

  /// Notifies all waiters (if any exists) that the condition has been met.
  ///
  /// Note: To avoid missed notification it is best hold the related mutex
  //        lock when calling notifyAll.
  void notifyAll();

private:
#ifdef SWIFT_RUNTIME_MUTEX_HAVE_PHTREADS
  pthread_cond_t PThreadCond;
#endif
};

/// Internal (private) implementation of mutex functionality.
/// Use Mutex instead (see below).
class MutexImpl {
  friend class Mutex;

public:
  MutexImpl() = delete;
  ~MutexImpl();

  MutexImpl(const MutexImpl &) = delete;
  MutexImpl &operator=(const MutexImpl &) = delete;
  MutexImpl(MutexImpl &&) = delete;
  MutexImpl &operator=(MutexImpl &&) = delete;

public:
  void lock();
  void unlock();
  bool try_lock();
  void wait(Condition &condition);

private:
  explicit MutexImpl(bool checked);

#ifdef SWIFT_RUNTIME_MUTEX_HAVE_PHTREADS
  pthread_mutex_t PThreadMutex;
#endif
};

/// Internal (private) implementation of scoped locking functionality.
/// Use ScopedLock instead (see below).
class ScopedLockImpl {
  friend class Mutex;
  friend class ScopedLock;

public:
  ScopedLockImpl() = delete;
  ~ScopedLockImpl() { Impl.unlock(); }

  ScopedLockImpl(const ScopedLockImpl &) = delete;
  ScopedLockImpl &operator=(const ScopedLockImpl &) = delete;
  ScopedLockImpl(ScopedLockImpl &&) = delete;
  ScopedLockImpl &operator=(ScopedLockImpl &&) = delete;

private:
  ScopedLockImpl(MutexImpl &impl) : Impl(impl) { Impl.lock(); }
  MutexImpl &Impl;
};

/// Internal (private) implementation of scoped unlocking functionality.
/// Use ScopedUnlock instead (see below).
class ScopedUnlockImpl {
  friend class Mutex;
  friend class ScopedUnlock;

public:
  ScopedUnlockImpl() = delete;
  ~ScopedUnlockImpl() { Impl.lock(); }

  ScopedUnlockImpl(const ScopedUnlockImpl &) = delete;
  ScopedUnlockImpl &operator=(const ScopedUnlockImpl &) = delete;
  ScopedUnlockImpl(ScopedUnlockImpl &&) = delete;
  ScopedUnlockImpl &operator=(ScopedUnlockImpl &&) = delete;

private:
  ScopedUnlockImpl(MutexImpl &impl) : Impl(impl) { Impl.unlock(); }
  MutexImpl &Impl;
};

/// A Mutex object that supports `BasicLockable` and `Lockable` C++ concepts.
/// See http://en.cppreference.com/w/cpp/concept/BasicLockable
/// See http://en.cppreference.com/w/cpp/concept/Lockable
///
/// This is NOT a recursive mutex.
class Mutex {
  friend class ScopedLock;
  friend class ScopedUnlock;

  Mutex(const Mutex &) = delete;
  Mutex &operator=(const Mutex &) = delete;
  Mutex(Mutex &&) = delete;
  Mutex &operator=(Mutex &&) = delete;

public:
  /// Constructs a non-recursive mutex.
  ///
  /// If `checked` is true the mutex will attempt to check for misuse and
  /// fatalError when detected. If `checked` is false (the default) the
  /// mutex will make little to no effort to check for misuse (e.g. efficient).
  explicit Mutex(bool checked = false) : Impl(checked) {}

public:
  /// The method lock() has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread until exclusive ownership of the mutex
  ///   can be obtained.
  /// - Prior m.unlock() operations on the same mutex synchronize-with
  ///   this lock operation.
  /// - The behavior is undefined if the calling thread already owns
  ///   the mutex (likely a deadlock).
  /// - Does not throw exceptions but will halt on error (fatalError).
  void lock() { Impl.lock(); }

  /// The method unlock() has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the mutex and
  ///   synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the mutex.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void unlock() { Impl.unlock(); }

  /// The method lock() has the following properties:
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
  bool try_lock() { return Impl.try_lock(); }

public:
  /// Releases lock, waits on supplied condition, and relocks before returning.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  void wait(Condition &condition) { Impl.wait(condition); }

public:
  /// Acquires lock before calling the supplied critical section and release
  /// lock on return from critical section.
  ///
  /// For example the following mutates value while holding the mutex lock.
  ///
  ///   mutex.lock([&value] { value++; });
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  template <typename CriticalSection>
  void lock(CriticalSection criticalSection) {
    ScopedLockImpl guard(Impl);
    criticalSection();
  }

  /// Acquires lock before calling the supplied critical section. If critical
  /// section returns `true` then it will wait on the supplied condition and
  /// call critical section again when wait returns (again holding the lock).
  /// If critical section returns `false` it will no longer wait, it will
  /// release the lock and return (e.g. lockOrWait returns).
  ///
  /// For example the following will loop waiting on condition until
  /// `value > 0`. It will then "consume" that value and stop looping.
  /// ...all while being correctly protected by mutex.
  ///
  ///   mutex.lockOrWait(condition, [&value] {
  ///     if (value > 0) {
  ///       value--;
  ///       return false;
  ///     }
  ///    return true;
  ///   });
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  template <typename CriticalSection>
  void lockOrWait(Condition &condition, CriticalSection criticalSection) {
    ScopedLockImpl guard(Impl);
    while (criticalSection()) {
      Impl.wait(condition);
    }
  }

  /// Acquires lock before calling the supplied critical section and on return
  /// from critical section it notifies one waiter of supplied condition and
  /// then releases the lock.
  ///
  /// For example the following mutates value while holding the mutex lock and
  /// then notifies one condition waiter about this change.
  ///
  ///   mutex.lockAndNotifyOne([&value] { value++; });
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  template <typename CriticalSection>
  void lockAndNotifyOne(Condition &condition, CriticalSection criticalSection) {
    ScopedLockImpl guard(Impl);
    criticalSection();
    condition.notifyOne();
  }

  /// Acquires lock before calling the supplied critical section and on return
  /// from critical section it notifies all waiters of supplied condition and
  /// then releases the lock.
  ///
  /// For example the following mutates value while holding the mutex lock and
  /// then notifies all condition waiters about this change.
  ///
  ///   mutex.lockAndNotifyAll([&value] { value++; });
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  template <typename CriticalSection>
  void lockAndNotifyAll(Condition &condition, CriticalSection criticalSection) {
    ScopedLockImpl guard(Impl);
    criticalSection();
    condition.notifyAll();
  }

private:
  MutexImpl Impl;
};

/// A stack based object that locks the supplied mutex on construction
/// and unlock it on destruction.
///
/// Precondition: Mutex unlocked by this thread, undefined otherwise.
class ScopedLock : ScopedLockImpl {
public:
  explicit ScopedLock(Mutex &mutex) : ScopedLockImpl(mutex.Impl) {}

  ScopedLock(const ScopedLock &) = delete;
  ScopedLock &operator=(const ScopedLock &) = delete;
  ScopedLock(ScopedLock &&) = delete;
  ScopedLock &operator=(ScopedLock &&) = delete;
};

/// A stack based object that unlocks the supplied mutex on construction
/// and relocks it on destruction.
///
/// Precondition: Mutex locked by this thread, undefined otherwise.
class ScopedUnlock : ScopedUnlockImpl {
public:
  explicit ScopedUnlock(Mutex &mutex) : ScopedUnlockImpl(mutex.Impl) {}

  ScopedUnlock(const ScopedUnlock &) = delete;
  ScopedUnlock &operator=(const ScopedUnlock &) = delete;
  ScopedUnlock(ScopedUnlock &&) = delete;
  ScopedUnlock &operator=(ScopedUnlock &&) = delete;
};
}

#endif
