//===----------- AtomicCache.swift - Atomically Initialized Cache ---------===//
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

import Foundation

/// AtomicCache is a wrapper class around an uninitialized value.
/// It takes a closure that it will use to create the value atomically. The
/// value is guaranteed to be set exactly one time, but the provided closure
/// may be called multiple times by threads racing to initialize the value.
/// Do not rely on the closure being called only one time.
class AtomicCache<Value: AnyObject> {
  /// The cached pointer that will be filled in the first time `value` is
  /// accessed.
  private var _cachedValue: AnyObject?

  /// The value inside this cache. If the value has not been initialized when
  /// this value is requested, then the closure will be called and its resulting
  /// value will be atomically compare-exchanged into the cache.
  /// If multiple threads access the value before initialization, they will all
  /// end up returning the correct, initialized value.
  /// - Parameter create: The closure that will return the fully realized value
  ///                     inside the cache.
  func value(_ create: () -> Value) -> Value {
    return withUnsafeMutablePointer(to: &_cachedValue) { ptr in
      // Perform an atomic load -- if we get a value, then return it.
      if let _cached = _stdlib_atomicLoadARCRef(object: ptr) {
        return _cached as! Value
      }

      // Otherwise, create the value...
      let value = create()

      // ...and attempt to initialize the pointer with that value.
      if _stdlib_atomicInitializeARCRef(object: ptr, desired: value) {
        // If we won the race, just return the value we made.
        return value
      }

      // Otherwise, perform _another_ load to get the up-to-date value,
      // and let the one we just made die.
      return _stdlib_atomicLoadARCRef(object: ptr) as! Value
    }
  }

  /// Unsafely attempts to load the value and cast it to the appropriate
  /// type.
  /// - note: Only for use in the debugger!
  @available(*, deprecated, message: "Only for use in the debugger.")
  var unsafeValue: Value? {
    return withUnsafeMutablePointer(to: &_cachedValue) {
      return _stdlib_atomicLoadARCRef(object: $0) as? Value
    }
  }
}
