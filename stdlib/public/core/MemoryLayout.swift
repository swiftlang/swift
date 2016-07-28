//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The memory layout of a type, describing its size, stride, and alignment.
public enum MemoryLayout<T> {

  /// The contiguous memory footprint of the type.
  ///
  /// The `size` property for a type `T` does not include any
  /// dynamically-allocated or "remote" storage. In particular,
  /// `MemoryLayout<T>.size`, when `T` is a class type, is the same regardless
  /// of how many stored properties `T` has.
  @_transparent
  public static var size: Int {
    return Int(Builtin.sizeof(T.self))
  }

  /// The number of bytes from the start of one instance to the start of the
  /// next, when stored in a contiguous array.
  ///
  /// This is the same as the number of bytes moved when an `UnsafePointer<T>`
  /// is incremented. The type may have a lower minimal alignment that trades
  /// runtime performance for space efficiency. The result is always positive.
  @_transparent
  public static var stride: Int {
    return Int(Builtin.strideof_nonzero(T.self))
  }

  /// The default memory alignment of the type.
  @_transparent
  public static var alignment: Int {
    return Int(Builtin.alignof(T.self))
  }
}

extension MemoryLayout {
  @_transparent
  public // @testable
  static func _ofInstance(_: @autoclosure () -> T) -> MemoryLayout<T>.Type {
    return MemoryLayout<T>.self
  }
}
