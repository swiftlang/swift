//===--- Mutex.h - Mutex, ConditionVariable, & ReadWriteLock ----*- C++ -*-===//
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
// Mutex, ConditionVariable, Read/Write lock, and Scoped lock abstractions
// for use in Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MUTEX_H
#define SWIFT_RUNTIME_MUTEX_H

#include <type_traits>

#if (defined(__APPLE__) || defined(__linux__) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__HAIKU__))
#include "swift/Runtime/MutexPThread.h"
#elif defined(_WIN32)
#include "swift/Runtime/MutexWin32.h"
#else
#error "Implement equivalent of MutexPThread.h/cpp for your platform."
#endif

namespace swift {

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
  friend class Mutex;

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
};

using ScopedNotifyOne = ScopedNotifyOneT<ConditionVariable>;
using ScopedNotifyAll = ScopedNotifyAllT<ConditionVariable>;

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

class Mutex;
class StaticMutex;

/// A stack based object that locks the supplied mutex on construction
/// and unlocks it on destruction.
///
/// Precondition: Mutex unlocked by this thread, undefined otherwise.
typedef ScopedLockT<Mutex, false> ScopedLock;
typedef ScopedLockT<StaticMutex, false> StaticScopedLock;

/// A stack based object that unlocks the supplied mutex on construction
/// and relocks it on destruction.
///
/// Precondition: Mutex locked by this thread, undefined otherwise.
typedef ScopedLockT<Mutex, true> ScopedUnlock;
typedef ScopedLockT<StaticMutex, true> StaticScopedUnlock;

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

  /// Releases lock, waits on supplied condition, and relocks before returning.
  ///
  /// Precondition: Mutex held by this thread, undefined otherwise.
  void wait(ConditionVariable &condition) {
    ConditionPlatformHelper::wait(condition.Handle, Handle);
  }

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
  auto withLock(CriticalSection criticalSection) -> decltype(criticalSection()){
    ScopedLock guard(*this);
    return criticalSection();
  }

  /// Acquires lock before calling the supplied critical section. If critical
  /// section returns `false` then it will wait on the supplied condition and
  /// call the critical section again when wait returns (after acquiring lock).
  /// If critical section returns `true` (done) it will no longer wait, it
  /// will release the lock and return (lockOrWait returns to caller).
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
                      CriticalSection criticalSection) {
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
                             CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    return withLock([&] {
      ScopedNotifyOne guard(condition);
      return criticalSection();
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
                             CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    return withLock([&] {
      ScopedNotifyAll guard(condition);
      return criticalSection();
    });
  }

private:
  MutexHandle Handle;
};

/// Compile time adjusted stack based object that locks/unlocks the supplied
/// ReadWriteLock type. Use the provided typedefs instead of this directly.
template <typename T, bool Read, bool Inverted> class ScopedRWLockT {

  ScopedRWLockT() = delete;
  ScopedRWLockT(const ScopedRWLockT &) = delete;
  ScopedRWLockT &operator=(const ScopedRWLockT &) = delete;
  ScopedRWLockT(ScopedRWLockT &&) = delete;
  ScopedRWLockT &operator=(ScopedRWLockT &&) = delete;

public:
  explicit ScopedRWLockT(T &l) : Lock(l) {
    if (Inverted) {
      if (Read) {
        Lock.readUnlock();
      } else {
        Lock.writeUnlock();
      }
    } else {
      if (Read) {
        Lock.readLock();
      } else {
        Lock.writeLock();
      }
    }
  }

  ~ScopedRWLockT() {
    if (Inverted) {
      if (Read) {
        Lock.readLock();
      } else {
        Lock.writeLock();
      }
    } else {
      if (Read) {
        Lock.readUnlock();
      } else {
        Lock.writeUnlock();
      }
    }
  }

private:
  T &Lock;
};

class ReadWriteLock;
class StaticReadWriteLock;

/// A stack based object that unlocks the supplied ReadWriteLock on
/// construction and locks it for reading on destruction.
///
/// Precondition: ReadWriteLock unlocked by this thread, undefined otherwise.
typedef ScopedRWLockT<ReadWriteLock, true, false> ScopedReadLock;
typedef ScopedRWLockT<StaticReadWriteLock, true, false> StaticScopedReadLock;

/// A stack based object that unlocks the supplied ReadWriteLock on
/// construction and locks it for reading on destruction.
///
/// Precondition: ReadWriteLock unlocked by this thread, undefined
/// otherwise.
typedef ScopedRWLockT<ReadWriteLock, true, true> ScopedReadUnlock;
typedef ScopedRWLockT<StaticReadWriteLock, true, true> StaticScopedReadUnlock;

/// A stack based object that unlocks the supplied ReadWriteLock on
/// construction and locks it for reading on destruction.
///
/// Precondition: ReadWriteLock unlocked by this thread, undefined otherwise.
typedef ScopedRWLockT<ReadWriteLock, false, false> ScopedWriteLock;
typedef ScopedRWLockT<StaticReadWriteLock, false, false> StaticScopedWriteLock;

/// A stack based object that unlocks the supplied ReadWriteLock on
/// construction and locks it for writing on destruction.
///
/// Precondition: ReadWriteLock unlocked by this thread, undefined otherwise.
typedef ScopedRWLockT<ReadWriteLock, false, true> ScopedWriteUnlock;
typedef ScopedRWLockT<StaticReadWriteLock, false, true> StaticScopedWriteUnlock;

/// A Read / Write lock object that has semantics similar to `BasicLockable`
/// and `Lockable` C++ concepts however it supports multiple concurrent
/// threads holding the reader lock as long as the write lock isn't held and
/// only one thread can hold the write local at the same time.
///
/// If you need static allocated ReadWriteLock use StaticReadWriteLock.
///
/// See http://en.cppreference.com/w/cpp/concept/BasicLockable
/// See http://en.cppreference.com/w/cpp/concept/Lockable
///
/// This is NOT a recursive mutex.
class ReadWriteLock {

  ReadWriteLock(const ReadWriteLock &) = delete;
  ReadWriteLock &operator=(const ReadWriteLock &) = delete;
  ReadWriteLock(ReadWriteLock &&) = delete;
  ReadWriteLock &operator=(ReadWriteLock &&) = delete;

public:
  ReadWriteLock() { ReadWriteLockPlatformHelper::init(Handle); }
  ~ReadWriteLock() { ReadWriteLockPlatformHelper::destroy(Handle); }

  /// The readLock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread while the write lock is held by another 
  ///   thread and once the read lock is acquired by the calling thread
  ///   other threads are prevented from acquiring the write lock.
  /// - Multiple threads can hold the read lock at the same time.
  /// - Prior unlock() operations on the same lock synchronize-with
  ///   this lock operation.
  /// - The behavior is undefined if the calling thread already owns
  ///   the read or write lock (likely a deadlock).
  /// - Does not throw exceptions but will halt on error (fatalError).
  ///
  /// Callers must not mutate the data protected by the ReadWriteLock while
  /// holding the read lock, the write lock must be used.
  void readLock() { ReadWriteLockPlatformHelper::readLock(Handle); }

  /// The try_readLock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Attempts to obtain the read lock without blocking the calling thread.
  ///   If ownership is not obtained, returns immediately. The function is
  ///   allowed to spuriously fail and return even if the lock is not
  ///   currently owned by another thread.
  /// - If try_readLock() succeeds, prior unlock() operations on the same
  ///   object synchronize-with this operation. unlock() does not synchronize
  ///   with a failed try_readLock().
  /// - The behavior is undefined if the calling thread already owns
  ///   the read or write lock (likely a deadlock)?
  /// - Does not throw exceptions but will halt on error (fatalError).
  ///
  /// Callers must not mutate the data protected by the ReadWriteLock while
  /// holding the read lock, the write lock must be used.
  bool try_readLock() {
    return ReadWriteLockPlatformHelper::try_readLock(Handle);
  }

  /// The readUnlock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the read lock
  ///   and synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the read lock.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void readUnlock() { ReadWriteLockPlatformHelper::readUnlock(Handle); }

  /// The writeLock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread while the write lock or a read lock is held
  ///   by another thread and once the write lock is acquired by the calling
  ///   thread other threads are prevented from acquiring the write lock or a
  ///   read lock.
  /// - Only one thread can hold the write lock at the same time.
  /// - Prior unlock() operations on the same lock synchronize-with
  ///   this lock operation.
  /// - The behavior is undefined if the calling thread already owns
  ///   the read or write lock (likely a deadlock).
  /// - Does not throw exceptions but will halt on error (fatalError).
  void writeLock() { ReadWriteLockPlatformHelper::writeLock(Handle); }

  /// The try_writeLock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Attempts to obtain the write lock without blocking the calling thread.
  ///   If ownership is not obtained, returns immediately. The function is
  ///   allowed to spuriously fail and return even if the lock is not
  ///   currently owned by another thread.
  /// - If try_writeLock() succeeds, prior unlock() operations on the same
  ///   object synchronize-with this operation. unlock() does not synchronize
  ///   with a failed try_writeLock().
  /// - The behavior is undefined if the calling thread already owns
  ///   the read or write lock (likely a deadlock)?
  /// - Does not throw exceptions but will halt on error (fatalError).
  bool try_writeLock() {
    return ReadWriteLockPlatformHelper::try_writeLock(Handle);
  }

  /// The writeUnlock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the write lock
  ///   and synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the write lock.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void writeUnlock() { ReadWriteLockPlatformHelper::writeUnlock(Handle); }

  /// Acquires read lock before calling the supplied critical section and
  /// releases lock on return from critical section. Callers must not mutate
  /// the data protected by the ReadWriteLock while holding the read lock, the
  /// write lock must be used.
  ///
  /// This call can block while waiting for the lock to become available.
  ///
  /// For example the following reads the cached value while holding
  /// the read lock.
  ///
  /// ```
  ///   rw.withReadLock([&value] { value = cachedValue; });
  /// ```
  ///
  /// Precondition: ReadWriteLock not held by this thread, undefined otherwise.
  template <typename CriticalSection>
  auto withReadLock(CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    ScopedReadLock guard(*this);
    return criticalSection();
  }

  /// Acquires write lock before calling the supplied critical section and
  /// releases lock on return from critical section.
  ///
  /// This call can block while waiting for the lock to become available.
  ///
  /// For example the following updates the cached value while holding
  /// the write lock.
  ///
  /// ```
  ///   rw.withWriteLock([&newValue] { cachedValue = newValue });
  /// ```
  ///
  /// Precondition: ReadWriteLock not held by this thread, undefined otherwise.
  template <typename CriticalSection>
  auto withWriteLock(CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    ScopedWriteLock guard(*this);
    return criticalSection();
  }

private:
  ReadWriteLockHandle Handle;
};

/// A static allocation variant of ConditionVariable.
///
/// Use ConditionVariable instead unless you need static allocation.
class StaticConditionVariable {
  friend class StaticMutex;

  StaticConditionVariable(const StaticConditionVariable &) = delete;
  StaticConditionVariable &operator=(const StaticConditionVariable &) = delete;
  StaticConditionVariable(StaticConditionVariable &&) = delete;
  StaticConditionVariable &operator=(StaticConditionVariable &&) = delete;

public:
#if SWIFT_CONDITION_SUPPORTS_CONSTEXPR
  constexpr
#endif
      StaticConditionVariable()
      : Handle(ConditionPlatformHelper::staticInit()) {
  }

  /// See ConditionVariable::notifyOne
  void notifyOne() { ConditionPlatformHelper::notifyOne(Handle); }

  /// See ConditionVariable::notifyAll
  void notifyAll() { ConditionPlatformHelper::notifyAll(Handle); }

private:
  ConditionHandle Handle;
};

using StaticScopedNotifyOne = ScopedNotifyOneT<StaticConditionVariable>;
using StaticScopedNotifyAll = ScopedNotifyAllT<StaticConditionVariable>;

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

  /// See Mutex::wait
  void wait(StaticConditionVariable &condition) {
    ConditionPlatformHelper::wait(condition.Handle, Handle);
  }

  /// See Mutex::lock
  template <typename CriticalSection>
  auto withLock(CriticalSection criticalSection) -> decltype(criticalSection()){
    StaticScopedLock guard(*this);
    return criticalSection();
  }

  /// See Mutex::withLockOrWait
  template <typename CriticalSection>
  void withLockOrWait(StaticConditionVariable &condition,
                      CriticalSection criticalSection) {
    withLock([&] {
      while (!criticalSection()) {
        wait(condition);
      }
    });
  }

  /// See Mutex::withLockThenNotifyOne
  template <typename CriticalSection>
  auto withLockThenNotifyOne(StaticConditionVariable &condition,
                             CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    return withLock([&] {
      StaticScopedNotifyOne guard(condition);
      return criticalSection();
    });
  }

  /// See Mutex::withLockThenNotifyAll
  template <typename CriticalSection>
  auto withLockThenNotifyAll(StaticConditionVariable &condition,
                             CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    return withLock([&] {
      StaticScopedNotifyAll guard(condition);
      return criticalSection();
    });
  }

private:
  MutexHandle Handle;
};

/// A static allocation variant of ReadWriteLock.
///
/// Use ReadWriteLock instead unless you need static allocation.
class StaticReadWriteLock {

  StaticReadWriteLock(const StaticReadWriteLock &) = delete;
  StaticReadWriteLock &operator=(const StaticReadWriteLock &) = delete;
  StaticReadWriteLock(StaticReadWriteLock &&) = delete;
  StaticReadWriteLock &operator=(StaticReadWriteLock &&) = delete;

public:
#if SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR
  constexpr
#endif
      StaticReadWriteLock()
      : Handle(ReadWriteLockPlatformHelper::staticInit()) {
  }

  /// See ReadWriteLock::readLock
  void readLock() { ReadWriteLockPlatformHelper::readLock(Handle); }

  /// See ReadWriteLock::try_readLock
  bool try_readLock() {
    return ReadWriteLockPlatformHelper::try_readLock(Handle);
  }

  /// See ReadWriteLock::readUnlock
  void readUnlock() { ReadWriteLockPlatformHelper::readUnlock(Handle); }

  /// See ReadWriteLock::writeLock
  void writeLock() { ReadWriteLockPlatformHelper::writeLock(Handle); }

  /// See ReadWriteLock::try_writeLock
  bool try_writeLock() {
    return ReadWriteLockPlatformHelper::try_writeLock(Handle);
  }

  /// See ReadWriteLock::writeUnlock
  void writeUnlock() { ReadWriteLockPlatformHelper::writeUnlock(Handle); }

  /// See ReadWriteLock::withReadLock
  template <typename CriticalSection>
  auto withReadLock(CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    StaticScopedReadLock guard(*this);
    return criticalSection();
  }

  /// See ReadWriteLock::withWriteLock
  template <typename CriticalSection>
  auto withWriteLock(CriticalSection criticalSection)
      -> decltype(criticalSection()) {
    StaticScopedWriteLock guard(*this);
    return criticalSection();
  }

private:
  ReadWriteLockHandle Handle;
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

private:
  MutexHandle Handle;
};

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

#if SWIFT_READWRITELOCK_SUPPORTS_CONSTEXPR
static_assert(std::is_literal_type<StaticReadWriteLock>::value,
              "StaticReadWriteLock must be literal type");
#else
// Your platform doesn't currently support statically allocated ReadWriteLocks
// you will possibly see global-constructors warnings
#endif
}

#endif
