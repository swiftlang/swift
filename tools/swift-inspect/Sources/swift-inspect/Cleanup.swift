//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A helper struct that manages the cleanup of some external resource.
/// The value is stored as an Optional, but presented as a non-optional.
/// Accessing the value before one has been set is a runtime error. The cleanup
/// function is called on the value if one has been set, when setting a new
/// value or when the struct is destroyed.
struct Cleanup<T>: ~Copyable {
  /// The underlying Optional storage for the value.
  private var _value: T?

  /// The wrapped value. A value must be set before any value is read here.
  var value: T {
    get {
      _value!
    }
    set {
      if let _value {
        cleanup(_value)
      }
      _value = newValue
    }
  }

  /// The function used to clean up the resource held by this struct.
  let cleanup: (T) -> Void

  init(cleanup: @escaping (T) -> Void) {
    self._value = nil
    self.cleanup = cleanup
  }

  deinit {
    if let _value {
      cleanup(_value)
    }
  }
}
