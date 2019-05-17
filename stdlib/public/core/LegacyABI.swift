//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file contains non-API (or underscored) declarations that are needed to
// be kept around for ABI compatibility

extension Unicode.UTF16 {
  @available(*, unavailable, renamed: "Unicode.UTF16.isASCII")
  @inlinable
  public static func _isASCII(_ x: CodeUnit) -> Bool  {
    return Unicode.UTF16.isASCII(x)
  }
}

@available(*, unavailable, renamed: "Unicode.UTF8.isASCII")
@inlinable
internal func _isASCII(_ x: UInt8) -> Bool {
  return Unicode.UTF8.isASCII(x)
}

@available(*, unavailable, renamed: "Unicode.UTF8.isContinuation")
@inlinable
internal func _isContinuation(_ x: UInt8) -> Bool {
  return UTF8.isContinuation(x)
}

extension Substring {
@available(*, unavailable, renamed: "Substring.base")
  @inlinable
  internal var _wholeString: String { return base }
}

extension String {
  @available(*, unavailable, renamed: "String.withUTF8")
  @inlinable
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8(body)
  }
}

extension Substring {
  @available(*, unavailable, renamed: "Substring.withUTF8")
  @inlinable
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8(body)
  }
}

extension BidirectionalCollection {
  /// Returns a subsequence, up to the given maximum length, containing the
  /// final elements of the collection.
  ///
  /// If the maximum length exceeds the number of elements in the collection,
  /// the result contains the entire collection.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.suffix(2))
  ///     // Prints "[4, 5]"
  ///     print(numbers.suffix(10))
  ///     // Prints "[1, 2, 3, 4, 5]"
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A subsequence terminating at the end of the collection with at
  ///   most `maxLength` elements.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*k*), where *k* is equal to
  ///   `maxLength`.
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  @available(macOS, obsoleted: 9999)
  @available(iOS, obsoleted: 9999)
  @available(tvOS, obsoleted: 9999)
  @available(watchOS, obsoleted: 9999)
  @inlinable
  public __consuming func suffix(_ maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let start = index(
      endIndex,
      offsetBy: -maxLength,
      limitedBy: startIndex) ?? startIndex
    return self[start..<endIndex]
  }
#else
  @available(*, unavailable)
  @inlinable
  public __consuming func suffix(_ maxLength: Int) -> SubSequence {
    _precondition(
      maxLength >= 0,
      "Can't take a suffix of negative length from a collection")
    let start = index(
      endIndex,
      offsetBy: -maxLength,
      limitedBy: startIndex) ?? startIndex
    return self[start..<endIndex]
  }
#endif

  /// Returns a subsequence containing all but the specified number of final
  /// elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in the
  /// collection, the result is an empty subsequence.
  ///
  ///     let numbers = [1, 2, 3, 4, 5]
  ///     print(numbers.dropLast(2))
  ///     // Prints "[1, 2, 3]"
  ///     print(numbers.dropLast(10))
  ///     // Prints "[]"
  ///
  /// - Parameter k: The number of elements to drop off the end of the
  ///   collection. `k` must be greater than or equal to zero.
  /// - Returns: A subsequence that leaves off `k` elements from the end.
  ///
  /// - Complexity: O(1) if the collection conforms to
  ///   `RandomAccessCollection`; otherwise, O(*k*), where *k* is the number of
  ///   elements to drop.
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  @available(macOS, obsoleted: 9999)
  @available(iOS, obsoleted: 9999)
  @available(tvOS, obsoleted: 9999)
  @available(watchOS, obsoleted: 9999)
  @inlinable
  public __consuming func dropLast(_ k: Int) -> SubSequence {
    _precondition(
      k >= 0, "Can't drop a negative number of elements from a collection")
    let end = index(
      endIndex,
      offsetBy: -k,
      limitedBy: startIndex) ?? startIndex
    return self[startIndex..<end]
  }
#else
  @available(*, unavailable)
  @inlinable
  public __consuming func dropLast(_ k: Int) -> SubSequence {
    _precondition(
      k >= 0, "Can't drop a negative number of elements from a collection")
    let end = index(
      endIndex,
      offsetBy: -k,
      limitedBy: startIndex) ?? startIndex
    return self[startIndex..<end]
  }
#endif
}

// This function is no longer used but must be kept for ABI compatibility
// because references to it may have been inlined.
@usableFromInline
internal func _branchHint(_ actual: Bool, expected: Bool) -> Bool {
  // The LLVM intrinsic underlying int_expect_Int1 now requires an immediate
  // argument for the expected value so we cannot call it here. This should
  // never be called in cases where performance matters, so just return the
  // value without any branch hint.
  return actual
}

extension String {
  @usableFromInline // Never actually used in inlinable code...
  internal func _nativeCopyUTF16CodeUnits(
    into buffer: UnsafeMutableBufferPointer<UInt16>,
    range: Range<String.Index>
  ) { fatalError() }
}
