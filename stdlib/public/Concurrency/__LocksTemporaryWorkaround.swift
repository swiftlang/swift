////===----------------------------------------------------------------------===//
////
//// This source file is part of the Swift.org open source project
////
//// Copyright (c) 2020 Apple Inc. and the Swift project authors
//// Licensed under Apache License v2.0 with Runtime Library Exception
////
//// See https://swift.org/LICENSE.txt for license information
//// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
////
////===----------------------------------------------------------------------===//

import Swift
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
import Darwin
#else
import Glibc
#endif

// FIXME: all of this must be removed; this is just to PoC things out...

public final class _Mutex {
  @usableFromInline
  var mutex: pthread_mutex_t = pthread_mutex_t()

  public init() {
    var attr: pthread_mutexattr_t = pthread_mutexattr_t()
    pthread_mutexattr_init(&attr)
    pthread_mutexattr_settype(&attr, Int32(PTHREAD_MUTEX_RECURSIVE))

    let error = pthread_mutex_init(&self.mutex, &attr)
    pthread_mutexattr_destroy(&attr)

    switch error {
    case 0:
      return
    default:
      fatalError("Could not create mutex: \(error)")
    }
  }

  deinit {
    pthread_mutex_destroy(&mutex)
  }

  @inlinable
  public func lock() {
    let error = pthread_mutex_lock(&self.mutex)

    switch error {
    case 0:
      return
    case EDEADLK:
      fatalError("Mutex could not be acquired because it would have caused a deadlock")
    default:
      fatalError("Failed with unspecified error: \(error)")
    }
  }

  @inlinable
  public func unlock() {
    let error = pthread_mutex_unlock(&self.mutex)

    switch error {
    case 0:
      return
    case EPERM:
      fatalError("Mutex could not be unlocked because it is not held by the current thread")
    default:
      fatalError("Unlock failed with unspecified error: \(error)")
    }
  }

  @inlinable
  public func tryLock() -> Bool {
    let error = pthread_mutex_trylock(&self.mutex)

    switch error {
    case 0:
      return true
    case EBUSY:
      return false
    case EDEADLK:
      fatalError("Mutex could not be acquired because it would have caused a deadlock")
    default:
      fatalError("Failed with unspecified error: \(error)")
    }
  }

  @inlinable
  public func synchronized<A>(_ f: () -> A) -> A {
    self.lock()

    defer {
      unlock()
    }

    return f()
  }

  @inlinable
  public func synchronized<A>(_ f: () throws -> A) throws -> A {
    self.lock()

    defer {
      unlock()
    }

    return try f()
  }
}

/// :nodoc: Not intended for general use.
public final class Condition {
  @usableFromInline
  var condition: pthread_cond_t = pthread_cond_t()

  public init() {
    let error = pthread_cond_init(&self.condition, nil)

    switch error {
    case 0:
      return
    default:
      fatalError("Condition could not be created: \(error)")
    }
  }

  deinit {
    pthread_cond_destroy(&condition)
  }

  @inlinable
  public func wait(_ mutex: _Mutex) {
    let error = pthread_cond_wait(&self.condition, &mutex.mutex)

    switch error {
    case 0:
      return
    case EPERM:
      fatalError("Wait failed, mutex is not owned by this thread")
    case EINVAL:
      fatalError("Wait failed, condition is not valid")
    default:
      fatalError("Wait failed with unspecified error: \(error)")
    }
  }

//  @inlinable
//  public func wait(_ mutex: _Mutex, atMost amount: TimeAmount) -> Bool {
////    clock_gettime(CLOCK_REALTIME, &now)
////    let reltime = sleep_til_this_absolute_time - now;
//
//    #if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
//    let time = TimeSpec.from(timeAmount: amount)
//    #else
//    var now = timespec()
//    clock_gettime(CLOCK_REALTIME, &now)
//    let time = now + TimeSpec.from(timeAmount: amount)
//    #endif
//    let error = withUnsafePointer(to: time) { p -> Int32 in
//      #if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
//      return pthread_cond_timedwait_relative_np(&condition, &mutex.mutex, p)
//      #else
//      return pthread_cond_timedwait(&condition, &mutex.mutex, p)
//      #endif
//    }
//
//    switch error {
//    case 0:
//      return true
//    case ETIMEDOUT:
//      return false
//    case EPERM:
//      fatalError("Wait failed, mutex is not owned by this thread")
//    case EINVAL:
//      fatalError("Wait failed, condition is not valid")
//    default:
//      fatalError("Wait failed with unspecified error: \(error)")
//    }
//  }

  @inlinable
  public func signal() {
    let error = pthread_cond_signal(&self.condition)

    switch error {
    case 0:
      return
    case EINVAL:
      fatalError("Signal failed, condition is not valid")
    default:
      fatalError("Signal failed with unspecified error: \(error)")
    }
  }

  @inlinable
  public func signalAll() {
    let error = pthread_cond_broadcast(&self.condition)

    switch error {
    case 0:
      return
    case EINVAL:
      fatalError("Signal failed, condition is not valid")
    default:
      fatalError("Signal failed with unspecified error: \(error)")
    }
  }
}

internal final class BlockingReceptacle<Value> {
  @usableFromInline
  let lock = _Mutex()
  @usableFromInline
  let notEmpty = Condition()

  private var _value: Value?

  /// Offer a value to the Receptacle -- only once. Further offers will result in Faults.
  ///
  /// # Warning
  /// - Faults: when offered a value a second time! This is considered a programmer error,
  ///           make sure to always correctly only offer a single value to the receptacle.
  func offerOnce(_ value: Value) {
    self.lock.synchronized {
      guard self._value == nil else {
        fatalError(
          "BlockingReceptacle can only be offered once. Already was offered [\(String(reflecting: self._value))] before, " +
            "and can not accept new offer: [\(value)]!"
        )
      }
      self._value = value
      self.notEmpty.signalAll()
    }
  }

  func offer(_ value: Value) {
    self.lock.synchronized {
      guard self._value == nil else {
        return
      }
      self._value = value
      self.notEmpty.signalAll()
    }
  }

//  /// Await the value to be set, or return `nil` if timeout passes and no value was set.
//  func wait(atMost timeout: TimeAmount) -> Value? {
//    let deadline = Deadline.fromNow(timeout)
//    return self.lock.synchronized { () -> Value? in
//      while deadline.hasTimeLeft() {
//        if let v = self._value {
//          return v
//        }
//        _ = self.notEmpty.wait(self.lock, atMost: deadline.timeLeft)
//      }
//      return nil
//    }
//  }

  func wait() -> Value {
    self.lock.synchronized { () -> Value in
      while true {
        if let v = self._value {
          return v
        }
        self.notEmpty.wait(self.lock)
      }
    }
  }
}
