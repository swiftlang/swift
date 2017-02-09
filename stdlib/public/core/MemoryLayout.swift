//===----------------------------------------------------------------------===//
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

/// The memory layout of a type, describing its size, stride, and alignment.
///
/// You can use `MemoryLayout` as a source of information about a type when
/// allocating or binding memory using unsafe pointers. The following example
/// declares a `Point` type with `x` and `y` coordinates and a Boolean
/// `isFilled` property.
///
///     struct Point {
///         let x: Double
///         let y: Double
///         let isFilled: Bool
///     }
///
/// The size, stride, and alignment of the `Point` type are accessible as
/// static properties of `MemoryLayout<Point>`.
///
///     // MemoryLayout<Point>.size == 17
///     // MemoryLayout<Point>.stride == 24
///     // MemoryLayout<Point>.alignment == 8
///
/// Always use a multiple of a type's `stride` instead of its `size` when
/// allocating memory or accounting for the distance between instances in
/// memory. This example allocates untyped, uninitialized memory with space
/// for four instances of `Point`.
///
///     let count = 4
///     let pointPointer = UnsafeMutableRawPointer.allocate(
///             bytes: count * MemoryLayout<Point>.stride,
///             alignedTo: MemoryLayout<Point>.alignment)
public enum MemoryLayout<T> {
  /// The contiguous memory footprint of `T`, in bytes.
  ///
  /// A type's size does not include any dynamically allocated or out of line
  /// storage. In particular, `MemoryLayout<T>.size`, when `T` is a class
  /// type, is the same regardless of how many stored properties `T` has.
  ///
  /// When allocating memory for multiple instances of `T` using an unsafe
  /// pointer, use a multiple of the type's stride instead of its size.
  ///
  /// - SeeAlso: `stride`
  @_transparent
  public static var size: Int {
    return Int(Builtin.sizeof(T.self))
  }

  /// The number of bytes from the start of one instance of `T` to the start of
  /// the next when stored in contiguous memory or in an `Array<T>`.
  ///
  /// This is the same as the number of bytes moved when an `UnsafePointer<T>`
  /// instance is incremented. `T` may have a lower minimal alignment that
  /// trades runtime performance for space efficiency. This value is always
  /// positive.
  @_transparent
  public static var stride: Int {
    return Int(Builtin.strideof(T.self))
  }

  /// The default memory alignment of `T`, in bytes.
  ///
  /// Use the `alignment` property for a type when allocating memory using an
  /// unsafe pointer. This value is always positive.
  @_transparent
  public static var alignment: Int {
    return Int(Builtin.alignof(T.self))
  }
}

extension MemoryLayout {
  /// Returns the contiguous memory footprint of the given instance.
  ///
  /// The result does not include any dynamically allocated or out of line
  /// storage. In particular, pointers and class instances all have the same
  /// contiguous memory footprint, regardless of the size of the referenced
  /// data.
  ///
  /// When you have a type instead of an instance, use the
  /// `MemoryLayout<T>.size` static property instead.
  ///
  ///     let x: Int = 100
  ///
  ///     // Finding the size of a value's type
  ///     let s = MemoryLayout.size(ofValue: x)
  ///     // s == 8
  ///
  ///     // Finding the size of a type directly
  ///     let t = MemoryLayout<Int>.size
  ///     // t == 8
  ///
  /// - Parameter value: A value representative of the type to describe.
  /// - Returns: The size, in bytes, of the given value's type.
  ///
  /// - SeeAlso: `MemoryLayout.size`
  @_transparent
  public static func size(ofValue value: T) -> Int {
    return MemoryLayout.size
  }

  /// Returns the number of bytes from the start of one instance of `T` to the
  /// start of the next when stored in contiguous memory or in an `Array<T>`.
  ///
  /// This is the same as the number of bytes moved when an `UnsafePointer<T>`
  /// instance is incremented. `T` may have a lower minimal alignment that
  /// trades runtime performance for space efficiency. The result is always
  /// positive.
  ///
  /// When you have a type instead of an instance, use the
  /// `MemoryLayout<T>.stride` static property instead.
  ///
  ///     let x: Int = 100
  ///
  ///     // Finding the stride of a value's type
  ///     let s = MemoryLayout.stride(ofValue: x)
  ///     // s == 8
  ///
  ///     // Finding the stride of a type directly
  ///     let t = MemoryLayout<Int>.stride
  ///     // t == 8
  ///
  /// - Parameter value: A value representative of the type to describe.
  /// - Returns: The stride, in bytes, of the given value's type.
  ///
  /// - SeeAlso: `MemoryLayout.stride`
  @_transparent
  public static func stride(ofValue value: T) -> Int {
    return MemoryLayout.stride
  }

  /// Returns the default memory alignment of `T`.
  ///
  /// Use a type's alignment when allocating memory using an unsafe pointer.
  ///
  /// When you have a type instead of an instance, use the
  /// `MemoryLayout<T>.stride` static property instead.
  ///
  ///     let x: Int = 100
  ///
  ///     // Finding the alignment of a value's type
  ///     let s = MemoryLayout.alignment(ofValue: x)
  ///     // s == 8
  ///
  ///     // Finding the alignment of a type directly
  ///     let t = MemoryLayout<Int>.alignment
  ///     // t == 8
  ///
  /// - Parameter value: A value representative of the type to describe.
  /// - Returns: The default memory alignment, in bytes, of the given value's
  ///   type. This value is always positive.
  ///
  /// - SeeAlso: `MemoryLayout.alignment`
  @_transparent
  public static func alignment(ofValue value: T) -> Int {
    return MemoryLayout.alignment
  }
}
