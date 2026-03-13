//===--- Mutex.h - Mutex and ScopedLock ----------------------- -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Provides a system-independent Mutex abstraction.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_MUTEX_H
#define SWIFT_THREADING_MUTEX_H

#include <type_traits>
#include <utility>

#include "ScopedLock.h"

#include "Impl.h"

namespace swift {

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
    threading_impl::mutex_init(Handle, checked);
  }
  ~Mutex() { threading_impl::mutex_destroy(Handle); }

  /// The lock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread until exclusive ownership of the mutex
  ///   can be obtained.
  /// - Prior m.unlock() operations on the same mutex synchronize-with
  ///   this lock operation.
  /// - The behavior is undefined if the calling thread already owns
  ///   the mutex (likely a deadlock).
  /// - Does not throw exceptions but will halt on error (fatalError).
  void lock() { threading_impl::mutex_lock(Handle); }

  /// The unlock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the mutex and
  ///   synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the mutex.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void unlock() { threading_impl::mutex_unlock(Handle); }

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
  bool try_lock() { return threading_impl::mutex_try_lock(Handle); }

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

protected:
  threading_impl::mutex_handle Handle;
};

/// An unsafe variant of the above (for use in the error handling path)
///
/// This is used to ensure that we can't infinitely recurse if the mutex
/// itself generates errors.
class UnsafeMutex : public Mutex {
public:
  UnsafeMutex() : Mutex() {}

  void lock() { threading_impl::mutex_unsafe_unlock(Handle); }
  void unlock() { threading_impl::mutex_unsafe_unlock(Handle); }
};

/// A lazily initialized variant of Mutex.
///
/// Use Mutex instead unless you need static allocation.  LazyMutex *may*
/// be entirely statically initialized, on some platforms, but on others
/// it might be a little larger than and slightly slower than Mutex.
class LazyMutex {

  LazyMutex(const LazyMutex &) = delete;
  LazyMutex &operator=(const LazyMutex &) = delete;
  LazyMutex(LazyMutex &&) = delete;
  LazyMutex &operator=(LazyMutex &&) = delete;

public:
  constexpr LazyMutex() : Handle(SWIFT_LAZY_MUTEX_INITIALIZER) {}

  // No destructor; this is intentional; this class is for STATIC allocation
  // and you don't need to delete mutexes on termination.

  /// See Mutex::lock
  void lock() { threading_impl::lazy_mutex_lock(Handle); }

  /// See Mutex::unlock
  void unlock() { threading_impl::lazy_mutex_unlock(Handle); }

  /// See Mutex::try_lock
  bool try_lock() { return threading_impl::lazy_mutex_try_lock(Handle); }

  /// See Mutex::withLock
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
  typedef ScopedLockT<LazyMutex, false> ScopedLock;

  /// A stack based object that unlocks the supplied mutex on construction
  /// and relocks it on destruction.
  ///
  /// Precondition: Mutex locked by this thread, undefined otherwise.
  typedef ScopedLockT<LazyMutex, true> ScopedUnlock;

protected:
  threading_impl::lazy_mutex_handle Handle;
};

/// An unsafe variant of the above (for use in the error handling path)
///
/// This is used to ensure that we can't infinitely recurse if the mutex
/// itself generates errors.
class LazyUnsafeMutex : public LazyMutex {
public:
  constexpr LazyUnsafeMutex() : LazyMutex() {}

  void lock() { threading_impl::lazy_mutex_unsafe_lock(Handle); }
  void unlock() { threading_impl::lazy_mutex_unsafe_unlock(Handle); }
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

/// A recursive variant of Mutex.
class RecursiveMutex {

  RecursiveMutex(const RecursiveMutex &) = delete;
  RecursiveMutex &operator=(const RecursiveMutex &) = delete;
  RecursiveMutex(RecursiveMutex &&) = delete;
  RecursiveMutex &operator=(RecursiveMutex &&) = delete;

public:
  /// Constructs a non-recursive mutex.
  ///
  /// If `checked` is true the mutex will attempt to check for misuse and
  /// fatalError when detected. If `checked` is false (the default) the
  /// mutex will make little to no effort to check for misuse (more efficient).
  explicit RecursiveMutex(bool checked = false) {
    threading_impl::recursive_mutex_init(Handle, checked);
  }
  ~RecursiveMutex() { threading_impl::recursive_mutex_destroy(Handle); }

  /// The lock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Blocks the calling thread until exclusive ownership of the mutex
  ///   can be obtained.
  /// - Prior m.unlock() operations on the same mutex synchronize-with
  ///   this lock operation.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void lock() { threading_impl::recursive_mutex_lock(Handle); }

  /// The unlock() method has the following properties:
  /// - Behaves as an atomic operation.
  /// - Releases the calling thread's ownership of the mutex and
  ///   synchronizes-with the subsequent successful lock operations on
  ///   the same object.
  /// - The behavior is undefined if the calling thread does not own
  ///   the mutex.
  /// - Does not throw exceptions but will halt on error (fatalError).
  void unlock() { threading_impl::recursive_mutex_unlock(Handle); }

protected:
  threading_impl::recursive_mutex_handle Handle;
};

} // namespace swift

#endif // SWIFT_THREADING_MUTEX_H
