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

namespace swift {

/// A Condition that works with Mutex to allow – as an example – multi-threaded
/// producers and consumers to signal each other in a safe way.
class Condition {
  friend class MutexImplementation;

public:
  explicit Condition();
  ~Condition();

public:
  /// Notifies one waiter (if any exists) that the condition has been met.
  ///
  /// Note: To avoid missed notification it is best hold the related mutex
  //        lock when calling notifyOne.
  void notifyOne() const;

  /// Notifies all waiters (if any exists) that the condition has been met.
  ///
  /// Note: To avoid missed notification it is best hold the related mutex
  //        lock when calling notifyAll.
  void notifyAll() const;

protected:
  void* Data;
};

/// Internal (private) implementation of mutex functionality.
/// Use Mutex or RecursiveMutex instead (see below).
class MutexImplementation {
  friend class Mutex;

public:
  MutexImplementation() = delete;
  ~MutexImplementation();

protected:
  explicit MutexImplementation(bool checked);

public:
  void lock() const;
  void unlock() const;
  bool try_lock() const;

public:
  void wait(const Condition& condition) const;

private:
  void* Data;
};

/// Internal (private) implementation of scoped locking functionality.
/// Use ScopedLock instead (see below).
class ScopedLockImplementation {
  friend class Mutex;

public:
  ScopedLockImplementation() = delete;
  ~ScopedLockImplementation() {
    Impl.unlock();
  }

protected:
  ScopedLockImplementation(const MutexImplementation& impl) : Impl(impl) {
    Impl.lock();
  }

public:
  void wait(const Condition& condition) const {
    Impl.wait(condition);
  }

private:
  const MutexImplementation& Impl;
};

/// Internal (private) implementation of scoped unlocking functionality.
/// Use ScopedUnlock instead (see below).
class ScopedUnlockImplementation {
  friend class Mutex;

public:
  ScopedUnlockImplementation() = delete;
  ~ScopedUnlockImplementation() {
    Impl.lock();
  }

protected:
  ScopedUnlockImplementation(const MutexImplementation& impl) : Impl(impl) {
    Impl.unlock();
  }

private:
  const MutexImplementation& Impl;
};

/// A Mutex object that supports `BasicLockable` and `Lockable` C++ concepts.
/// See http://en.cppreference.com/w/cpp/concept/BasicLockable
/// See http://en.cppreference.com/w/cpp/concept/Lockable
///
/// This is NOT a recursive mutex.
class Mutex {
  friend class ScopedLock;
  friend class ScopedUnlock;

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
  void lock() const {
    Impl.lock();
  }

  /// The method unlock() has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the mutex and
  ///   synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the mutex.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void unlock() const {
    Impl.unlock();
  }

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
  bool try_lock() const {
    return Impl.try_lock();
  }

public:
  /// Releases lock, waits on supplied condition, and relocks before returning.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  void wait(const Condition& condition) const {
    Impl.wait(condition);
  }

public:
  /// Acquires lock before calling the supplied critical section and release
  /// lock on return from critical section.
  ///
  /// For example the following mutates value while holding the mutex lock.
  ///
  ///   mutex.lock([&value] { value++; });
  ///
  /// Precondition: Mutex unlocked by this thread, undefined otherwise.
  template<typename CriticalSection>
  void lock(CriticalSection criticalSection) const {
    ScopedLockImplementation guard(Impl);
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
  template<typename CriticalSection>
  void lockOrWait(const Condition& condition,
                  CriticalSection criticalSection) const {
    ScopedLockImplementation guard(Impl);
    while ( criticalSection() ) {
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
  template<typename CriticalSection>
  void lockAndNotifyOne(const Condition& condition,
                        CriticalSection criticalSection) const {
    ScopedLockImplementation guard(Impl);
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
  template<typename CriticalSection>
  void lockAndNotifyAll(const Condition& condition,
                        CriticalSection criticalSection) const {
    ScopedLockImplementation guard(Impl);
    criticalSection();
    condition.notifyAll();
  }

protected:
  MutexImplementation Impl;
};

/// A stack based object that locks the supplied mutex on construction
/// and unlock it on destruction.
///
/// Precondition: Mutex unlocked by this thread, undefined otherwise.
class ScopedLock : ScopedLockImplementation {
public:
  ScopedLock(Mutex& mutex) : ScopedLockImplementation(mutex.Impl) {}

public:
  /// Releases lock, waits on supplied condition, and relocks before returning.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  void wait(const Condition& condition) const {
    ScopedLockImplementation::wait(condition);
  }
};

/// A stack based object that unlocks the supplied mutex on construction
/// and relocks it on destruction.
///
/// Precondition: Mutex locked by this thread, undefined otherwise.
class ScopedUnlock : ScopedUnlockImplementation {
public:
  ScopedUnlock(Mutex& mutex) : ScopedUnlockImplementation(mutex.Impl) {}
};

}

#endif
