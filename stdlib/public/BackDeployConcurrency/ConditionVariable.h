//===--- ConditionVariable.h - A condition variable -------------*- C++ -*-===//
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
// ConditionVariable abstraction for use in Swift back-deployed concurrency
// runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CONDITION_VARIABLE_H
#define SWIFT_RUNTIME_CONDITION_VARIABLE_H

#include "swift/Threading/Mutex.h"
#include <type_traits>
#include <utility>

#if __has_include(<unistd.h>)
#include <unistd.h>
#endif

namespace swift {

#define SWIFT_CONDITION_SUPPORTS_CONSTEXPR 1

typedef pthread_cond_t ConditionHandle;
typedef pthread_mutex_t ConditionMutexHandle;

/// PThread low-level implementation that supports ConditionVariable
/// found in Mutex.h
///
/// See ConditionVariable
struct ConditionPlatformHelper {
#if SWIFT_CONDITION_SUPPORTS_CONSTEXPR
  static constexpr
#else
  static
#endif
      ConditionHandle
      staticInit() {
    return PTHREAD_COND_INITIALIZER;
  };
  static void init(ConditionHandle &condition);
  static void destroy(ConditionHandle &condition);
  static void notifyOne(ConditionHandle &condition);
  static void notifyAll(ConditionHandle &condition);
  static void wait(ConditionHandle &condition, ConditionMutexHandle &mutex);
};

/// A stack based object that notifies one thread waiting on a condition
/// variable on destruction.
template <typename ConditionVariable>
class ScopedNotifyOneT {
  ScopedNotifyOneT() = delete;
  ScopedNotifyOneT(const ScopedNotifyOneT &) = delete;
  ScopedNotifyOneT &operator=(const ScopedNotifyOneT &) = delete;

  ConditionVariable &Condition;
public:
  explicit ScopedNotifyOneT(ConditionVariable &c) : Condition(c) {}

  ~ScopedNotifyOneT() {
    Condition.notifyOne();
  }
};

/// A stack based object that notifies all threads waiting on a condition
/// variable on destruction.
template <typename ConditionVariable>
class ScopedNotifyAllT {
  ScopedNotifyAllT() = delete;
  ScopedNotifyAllT(const ScopedNotifyAllT &) = delete;
  ScopedNotifyAllT &operator=(const ScopedNotifyAllT &) = delete;

  ConditionVariable &Condition;
public:
  explicit ScopedNotifyAllT(ConditionVariable &c) : Condition(c) {}

  ~ScopedNotifyAllT() {
    Condition.notifyAll();
  }
};

/// A ConditionVariable that works with Mutex to allow -- as an example --
/// multi-threaded producers and consumers to signal each other in a safe way.
class ConditionVariable {
  friend class ConditionMutex;
  friend class StaticConditionVariable;

  ConditionVariable(const ConditionVariable &) = delete;
  ConditionVariable &operator=(const ConditionVariable &) = delete;
  ConditionVariable(ConditionVariable &&) = delete;
  ConditionVariable &operator=(ConditionVariable &&) = delete;

public:
  ConditionVariable() { ConditionPlatformHelper::init(Handle); }
  ~ConditionVariable() { ConditionPlatformHelper::destroy(Handle); }

  /// Notifies one waiter (if any exists) that the condition has been met.
  ///
  /// Note: To avoid missed notification it is best hold the related mutex
  //        lock when calling notifyOne.
  void notifyOne() { ConditionPlatformHelper::notifyOne(Handle); }

  /// Notifies all waiters (if any exists) that the condition has been met.
  ///
  /// Note: To avoid missed notification it is best hold the related mutex
  //        lock when calling notifyAll.
  void notifyAll() { ConditionPlatformHelper::notifyAll(Handle); }

private:
  ConditionHandle Handle;

public:
  /// A Mutex object that also supports ConditionVariables.
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
    /// mutex will make little to no effort to check for misuse (more
    /// efficient).
    explicit Mutex(bool checked = false);
    ~Mutex();

    /// The lock() method has the following properties:
    /// - Behaves as an atomic operation.
    /// - Blocks the calling thread until exclusive ownership of the mutex
    ///   can be obtained.
    /// - Prior m.unlock() operations on the same mutex synchronize-with
    ///   this lock operation.
    /// - The behavior is undefined if the calling thread already owns
    ///   the mutex (likely a deadlock).
    /// - Does not throw exceptions but will halt on error (fatalError).
    void lock();

    /// The unlock() method has the following properties:
    /// - Behaves as an atomic operation.
    /// - Releases the calling thread's ownership of the mutex and
    ///   synchronizes-with the subsequent successful lock operations on
    ///   the same object.
    /// - The behavior is undefined if the calling thread does not own
    ///   the mutex.
    /// - Does not throw exceptions but will halt on error (fatalError).
    void unlock();

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
    bool try_lock();

    /// Releases lock, waits on supplied condition, and relocks before
    /// returning.
    ///
    /// Precondition: Mutex held by this thread, undefined otherwise.
    void wait(ConditionVariable &condition);

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

    /// Acquires lock before calling the supplied critical section. If critical
    /// section returns `false` then it will wait on the supplied condition and
    /// call the critical section again when wait returns (after acquiring
    /// lock). If critical section returns `true` (done) it will no longer wait,
    /// it will release the lock and return (lockOrWait returns to caller).
    ///
    /// This call can block while waiting for the lock to become available.
    ///
    /// For example the following will loop waiting on the condition until
    /// `value > 0`. It will then "consume" that value and stop looping.
    /// ...all while being correctly protected by mutex.
    ///
    /// ```
    ///   mutex.withLockOrWait(condition, [&value] {
    ///     if (value > 0) {
    ///       value--;
    ///       return true;
    ///     }
    ///    return false;
    ///   });
    /// ```
    ///
    /// Precondition: Mutex not held by this thread, undefined otherwise.
    template <typename CriticalSection>
    void withLockOrWait(ConditionVariable &condition,
                        CriticalSection &&criticalSection) {
      withLock([&] {
        while (!criticalSection()) {
          wait(condition);
        }
      });
    }

    /// Acquires lock before calling the supplied critical section and on return
    /// from critical section it notifies one waiter of supplied condition and
    /// then releases the lock.
    ///
    /// This call can block while waiting for the lock to become available.
    ///
    /// For example the following mutates value while holding the mutex lock and
    /// then notifies one condition waiter about this change.
    ///
    /// ```
    ///   mutex.withLockThenNotifyOne(condition, [&value] { value++; });
    /// ```
    ///
    /// Precondition: Mutex not held by this thread, undefined otherwise.
    template <typename CriticalSection>
    auto withLockThenNotifyOne(ConditionVariable &condition,
                               CriticalSection &&criticalSection)
        -> decltype(std::forward<CriticalSection>(criticalSection)()) {
      return withLock([&] {
        ScopedNotifyOne guard(condition);
        return std::forward<CriticalSection>(criticalSection)();
      });
    }

    /// Acquires lock before calling the supplied critical section and on return
    /// from critical section it notifies all waiters of supplied condition and
    /// then releases the lock.
    ///
    /// This call can block while waiting for the lock to become available.
    ///
    /// For example the following mutates value while holding the mutex lock and
    /// then notifies all condition waiters about this change.
    ///
    /// ```
    ///   mutex.withLockThenNotifyAll(condition, [&value] { value++; });
    /// ```
    ///
    /// Precondition: Mutex not held by this thread, undefined otherwise.
    template <typename CriticalSection>
    auto withLockThenNotifyAll(ConditionVariable &condition,
                               CriticalSection &&criticalSection)
        -> decltype(std::forward<CriticalSection>(criticalSection)()) {
      return withLock([&] {
        ScopedNotifyAll guard(condition);
        return std::forward<CriticalSection>(criticalSection)();
      });
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
    ConditionMutexHandle Handle;
  };

  using ScopedNotifyOne = ScopedNotifyOneT<ConditionVariable>;
  using ScopedNotifyAll = ScopedNotifyAllT<ConditionVariable>;
};

}

#endif
