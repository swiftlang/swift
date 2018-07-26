//===--- StringUTF8.swift - A UTF8 view of String -------------------------===//
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

extension String {
  /// A view of a string's contents as a collection of UTF-8 code units.
  ///
  /// You can access a string's view of UTF-8 code units by using its `utf8`
  /// property. A string's UTF-8 view encodes the string's Unicode scalar
  /// values as 8-bit integers.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.utf8 {
  ///         print(v)
  ///     }
  ///     // 70
  ///     // 108
  ///     // 111
  ///     // 119
  ///     // 101
  ///     // 114
  ///     // 115
  ///     // 32
  ///     // 240
  ///     // 159
  ///     // 146
  ///     // 144
  ///
  /// A string's Unicode scalar values can be up to 21 bits in length. To
  /// represent those scalar values using 8-bit integers, more than one UTF-8
  /// code unit is often required.
  ///
  ///     let flowermoji = "💐"
  ///     for v in flowermoji.unicodeScalars {
  ///         print(v, v.value)
  ///     }
  ///     // 💐 128144
  ///
  ///     for v in flowermoji.utf8 {
  ///         print(v)
  ///     }
  ///     // 240
  ///     // 159
  ///     // 146
  ///     // 144
  ///
  /// In the encoded representation of a Unicode scalar value, each UTF-8 code
  /// unit after the first is called a *continuation byte*.
  ///
  /// UTF8View Elements Match Encoded C Strings
  /// =========================================
  ///
  /// Swift streamlines interoperation with C string APIs by letting you pass a
  /// `String` instance to a function as an `Int8` or `UInt8` pointer. When you
  /// call a C function using a `String`, Swift automatically creates a buffer
  /// of UTF-8 code units and passes a pointer to that buffer. The code units
  /// of that buffer match the code units in the string's `utf8` view.
  ///
  /// The following example uses the C `strncmp` function to compare the
  /// beginning of two Swift strings. The `strncmp` function takes two
  /// `const char*` pointers and an integer specifying the number of characters
  /// to compare. Because the strings are identical up to the 14th character,
  /// comparing only those characters results in a return value of `0`.
  ///
  ///     let s1 = "They call me 'Bell'"
  ///     let s2 = "They call me 'Stacey'"
  ///
  ///     print(strncmp(s1, s2, 14))
  ///     // Prints "0"
  ///     print(String(s1.utf8.prefix(14)))
  ///     // Prints "They call me '"
  ///
  /// Extending the compared character count to 15 includes the differing
  /// characters, so a nonzero result is returned.
  ///
  ///     print(strncmp(s1, s2, 15))
  ///     // Prints "-17"
  ///     print(String(s1.utf8.prefix(15)))
  ///     // Prints "They call me 'B"
  @_fixed_layout
  public struct UTF8View {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable @inline(__always)
    internal init(_ guts: _StringGuts) {
      self._guts = guts
      _invariantCheck()
    }
  }
}

extension String.UTF8View {
  @inlinable @inline(__always)
  internal func _invariantCheck() {
    #if INTERNAL_CHECKS_ENABLED
    #endif
  }
}

// TODO(UTF8 merge): when this refactoring lands on master and we can get a
// toolchain, remove these and use the single-underscore ones. Will still need
// to solve access control somehow, perhaps shims need to expose them...
extension BidirectionalCollection {
  /// Do not use this method directly; call advanced(by: n) instead.
  @inlinable
  @inline(__always)
  internal func __advanceForward(_ i: Index, by n: Int) -> Index {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")

    var i = i
    for _ in stride(from: 0, to: n, by: 1) {
      formIndex(after: &i)
    }
    return i
  }

  /// Do not use this method directly; call advanced(by: n, limit) instead.
  @inlinable
  @inline(__always)
  internal func __advanceForward(
    _ i: Index, by n: Int, limitedBy limit: Index
  ) -> Index? {
    _precondition(n >= 0,
      "Only BidirectionalCollections can be advanced by a negative amount")

    var i = i
    for _ in stride(from: 0, to: n, by: 1) {
      if i == limit {
        return nil
      }
      formIndex(after: &i)
    }
    return i
  }

  @inlinable // FIXME(sil-serialize-all)
  public func __index(_ i: Index, offsetBy n: Int) -> Index {
    if n >= 0 {
      return __advanceForward(i, by: n)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      formIndex(before: &i)
    }
    return i
  }

  @inlinable // FIXME(sil-serialize-all)
  public func __index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    if n >= 0 {
      return __advanceForward(i, by: n, limitedBy: limit)
    }
    var i = i
    for _ in stride(from: 0, to: n, by: -1) {
      if i == limit {
        return nil
      }
      formIndex(before: &i)
    }
    return i
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func __distance(from start: Index, to end: Index) -> Int {
    var start = start
    var count = 0

    if start < end {
      while start != end {
        count += 1
        formIndex(after: &start)
      }
    }
    else if start > end {
      while start != end {
        count -= 1
        formIndex(before: &start)
      }
    }

    return count
  }
}

extension String.UTF8View: BidirectionalCollection {
  public typealias Index = String.Index

  public typealias Element = UTF8.CodeUnit

  /// The position of the first code unit if the UTF-8 view is
  /// nonempty.
  ///
  /// If the UTF-8 view is empty, `startIndex` is equal to `endIndex`.
  @inlinable
  public var startIndex: Index {
    @inline(__always) get { return Index(encodedOffset: 0) }
  }

  /// The "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// In an empty UTF-8 view, `endIndex` is equal to `startIndex`.
  @inlinable
  public var endIndex: Index {
    @inline(__always) get { return Index(encodedOffset: _guts.count) }
  }

  /// Returns the next consecutive position after `i`.
  ///
  /// - Precondition: The next position is representable.
  @inlinable @inline(__always)
  public func index(after i: Index) -> Index {
    if _fastPath(_guts.isFastUTF8) {
      return Index(encodedOffset: i.encodedOffset &+ 1)
    }

    return _foreignIndex(after: i)
  }

  @inlinable @inline(__always)
  public func index(before i: Index) -> Index {
    precondition(i.encodedOffset > 0)
    if _fastPath(_guts.isFastUTF8) {
      return Index(encodedOffset: i.encodedOffset &- 1)
    }

    return _foreignIndex(before: i)
  }

  @inlinable @inline(__always)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    if _fastPath(_guts.isFastUTF8) {
      let offset = i.encodedOffset + n
      _precondition(offset >= 0 && offset <= _guts.count)
      return Index(encodedOffset: offset)
    }

    return _foreignIndex(i, offsetBy: n)
  }

  @inlinable @inline(__always)
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    if _fastPath(_guts.isFastUTF8) {
      // Check the limit: ignore limit if it precedes `i` (in the correct
      // direction), otherwise must not be beyond limit (in the correct
      // direction).
      let iOffset = i.encodedOffset
      let result = iOffset + n
      let limitOffset = limit.encodedOffset
      if n >= 0 {
        guard limitOffset < iOffset || result <= limitOffset else { return nil }
      } else {
        guard limitOffset > iOffset || result >= limitOffset else { return nil }
      }
      return Index(encodedOffset: result)
    }

    return _foreignIndex(i, offsetBy: n, limitedBy: limit)
  }

  @inlinable @inline(__always)
  public func distance(from i: Index, to j: Index) -> Int {
    if _fastPath(_guts.isFastUTF8) {
      return j.encodedOffset &- i.encodedOffset
    }
    return _foreignDistance(from: i, to: j)
  }

  /// Accesses the code unit at the given position.
  ///
  /// The following example uses the subscript to print the value of a
  /// string's first UTF-8 code unit.
  ///
  ///     let greeting = "Hello, friend!"
  ///     let i = greeting.utf8.startIndex
  ///     print("First character's UTF-8 code unit: \(greeting.utf8[i])")
  ///     // Prints "First character's UTF-8 code unit: 72"
  ///
  /// - Parameter position: A valid index of the view. `position`
  ///   must be less than the view's end index.
  @inlinable
  public subscript(i: Index) -> UTF8.CodeUnit {
    @inline(__always) get {
      _precondition(i.encodedOffset >= 0 && i < endIndex)
      if _fastPath(_guts.isFastUTF8) {
        return _guts.withFastUTF8 { utf8 in utf8[i.encodedOffset] }
      }

      return _foreignSubscript(position: i)
    }
  }
}

extension String.UTF8View: CustomStringConvertible {
 @inlinable
 public var description: String {
   @inline(__always) get { return String(String(_guts)) }
 }
}

extension String.UTF8View: CustomDebugStringConvertible {
 public var debugDescription: String {
   return "UTF8View(\(self.description.debugDescription))"
 }
}


extension String {
  /// A UTF-8 encoding of `self`.
  @inlinable
  public var utf8: UTF8View {
    @inline(__always) get { return UTF8View(self._guts) }
    set {
      unimplemented_utf8()
    }
  }

  /// A contiguously stored null-terminated UTF-8 representation of the string.
  ///
  /// To access the underlying memory, invoke `withUnsafeBufferPointer` on the
  /// array.
  ///
  ///     let s = "Hello!"
  ///     let bytes = s.utf8CString
  ///     print(bytes)
  ///     // Prints "[72, 101, 108, 108, 111, 33, 0]"
  ///
  ///     bytes.withUnsafeBufferPointer { ptr in
  ///         print(strlen(ptr.baseAddress!))
  ///     }
  ///     // Prints "6"
  public var utf8CString: ContiguousArray<CChar> {
    if _fastPath(_guts.isFastUTF8) {
      var result = _guts.withFastUTF8 { return ContiguousArray($0._asCChar) }
      result.append(0)
      return result
    }

    return _slowUTF8CString()
  }

  @usableFromInline @inline(never) // slow-path
  internal func _slowUTF8CString() -> ContiguousArray<CChar> {
    var result = ContiguousArray<CChar>()
    result.reserveCapacity(self._guts.count + 1)
    for c in self.utf8 {
      result.append(CChar(bitPattern: c))
    }
    result.append(0)
    return result
  }

  /// Creates a string corresponding to the given sequence of UTF-8 code units.
  @available(swift, introduced: 4.0, message:
  "Please use failable String.init?(_:UTF8View) when in Swift 3.2 mode")
  @inlinable @inline(__always)
  public init(_ utf8: UTF8View) {
    self = String(utf8._guts)
  }
}

// TODO(UTF8): design specialized iterator, rather than default indexing one
//extension String.UTF8View {
//  @_fixed_layout // FIXME(sil-serialize-all)
//  public struct Iterator {
//    // TODO(UTF8):
//  }
//
//  public func makeIterator() -> Iterator {
//    unimplemented_utf8()
//  }
//}
//
//extension String.UTF8View.Iterator : IteratorProtocol {
//  public typealias Element = String.UTF8View.Element
//
//  @inlinable @inline(__always)
//  public mutating func next() -> Unicode.UTF8.CodeUnit? {
//    unimplemented_utf8()
//  }
//}

extension String.UTF8View {
  @inlinable
  public var count: Int {
    @inline(__always) get {
      if _fastPath(_guts.isFastUTF8) {
        return _guts.count
      }
      return _foreignCount()
    }
  }
}

// Index conversions
extension String.UTF8View.Index {
  /// Creates an index in the given UTF-8 view that corresponds exactly to the
  /// specified `UTF16View` position.
  ///
  /// The following example finds the position of a space in a string's `utf16`
  /// view and then converts that position to an index in the string's
  /// `utf8` view.
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let utf16Index = cafe.utf16.firstIndex(of: 32)!
  ///     let utf8Index = String.UTF8View.Index(utf16Index, within: cafe.utf8)!
  ///
  ///     print(Array(cafe.utf8[..<utf8Index]))
  ///     // Prints "[67, 97, 102, 195, 169]"
  ///
  /// If the position passed in `utf16Index` doesn't have an exact
  /// corresponding position in `utf8`, the result of the initializer is
  /// `nil`. For example, because UTF-8 and UTF-16 represent high Unicode code
  /// points differently, an attempt to convert the position of the trailing
  /// surrogate of a UTF-16 surrogate pair fails.
  ///
  /// The next example attempts to convert the indices of the two UTF-16 code
  /// points that represent the teacup emoji (`"🍵"`). The index of the lead
  /// surrogate is successfully converted to a position in `utf8`, but the
  /// index of the trailing surrogate is not.
  ///
  ///     let emojiHigh = cafe.utf16.index(after: utf16Index)
  ///     print(String.UTF8View.Index(emojiHigh, within: cafe.utf8))
  ///     // Prints "Optional(String.Index(...))"
  ///
  ///     let emojiLow = cafe.utf16.index(after: emojiHigh)
  ///     print(String.UTF8View.Index(emojiLow, within: cafe.utf8))
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in a `String` or one of its views.
  ///   - target: The `UTF8View` in which to find the new position.
  @inlinable // FIXME(sil-serialize-all)
  public init?(_ idx: String.Index, within target: String.UTF8View) {
    unimplemented_utf8()
  }
}

// Reflection
extension String.UTF8View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-8 view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

// TODO(UTF8): Can we just unify this view?
//===--- Slicing Support --------------------------------------------------===//
/// In Swift 3.2, in the absence of type context,
///
///   someString.utf8[someString.utf8.startIndex..<someString.utf8.endIndex]
///
/// was deduced to be of type `String.UTF8View`.  Provide a more-specific
/// Swift-3-only `subscript` overload that continues to produce
/// `String.UTF8View`.
extension String.UTF8View {
  public typealias SubSequence = Substring.UTF8View

  @inlinable
  @available(swift, introduced: 4)
  public subscript(r: Range<Index>) -> String.UTF8View.SubSequence {
    return Substring.UTF8View(self, _bounds: r)
  }
}

extension String.UTF8View {
  /// Copies `self` into the supplied buffer.
  ///
  /// - Precondition: The memory in `self` is uninitialized. The buffer must
  ///   contain sufficient uninitialized memory to accommodate
  ///   `source.underestimatedCount`.
  ///
  /// - Postcondition: The `Pointee`s at `buffer[startIndex..<returned index]`
  ///   are initialized.
  @inlinable @inline(__always)
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator, UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    guard buffer.baseAddress != nil else {
        _preconditionFailure(
          "Attempt to copy string contents into nil buffer pointer")
    }
    guard let written = _guts.copyUTF8(into: buffer) else {
      _preconditionFailure(
        "Insufficient space allocated to copy string contents")
    }

    let it = String().utf8.makeIterator()
    return (it, buffer.index(buffer.startIndex, offsetBy: written))
  }
}

// Foreign string support
extension String.UTF8View {
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(after i: Index) -> Index {
    _sanityCheck(_guts.isForeign)

    let cu = _guts.foreignUTF16CodeUnit(at: i.encodedOffset)
    let len = _numTranscodedUTF8CodeUnits(cu)

    if len == 1 {
      _sanityCheck(i.transcodedOffset == 0)
      return Index(encodedOffset: i.encodedOffset + 1)
    }

    // Check if we're still transcoding sub-scalar
    if i.transcodedOffset < len - 1 {
        return Index(transcodedAfter: i)
    }

    // Skip to the next scalar
    let scalarLen = len == 4 ? 2 : 1
    return Index(encodedOffset: i.encodedOffset + scalarLen)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(before i: Index) -> Index {
    _sanityCheck(_guts.isForeign)

    if i.transcodedOffset != 0 {
      _sanityCheck((1...3) ~= i.transcodedOffset)
      return Index(transcodedBefore: i)
    }
    var offset = i.encodedOffset &- 1
    var cu = _guts.foreignUTF16CodeUnit(at: offset)
    if _isTrailingSurrogate(cu) {
      offset = offset &- 1
      _sanityCheck(offset >= 0)
      cu = _guts.foreignUTF16CodeUnit(at: offset)
    }
    let len = _numTranscodedUTF8CodeUnits(cu)

    return Index(encodedOffset: offset, transcodedOffset: len &- 1)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignSubscript(position i: Index) -> UTF8.CodeUnit {
    _sanityCheck(_guts.isForeign)

    // Currently, foreign means NSString

    // TODO(UTF8 perf): Could probably work just off a single code unit
    let scalar = _guts.foreignScalar(startingAt: i.encodedOffset)
    let encoded = Unicode.UTF8.encode(scalar)._unsafelyUnwrappedUnchecked

    _sanityCheck(i.transcodedOffset < 1+encoded.count)
    return encoded[
      encoded.index(encoded.startIndex, offsetBy: i.transcodedOffset)]
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(_ i: Index, offsetBy n: Int) -> Index {
    _sanityCheck(_guts.isForeign)
    return __index(i, offsetBy: n)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    _sanityCheck(_guts.isForeign)
    return __index(i, offsetBy: n, limitedBy: limit)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignDistance(from i: Index, to j: Index) -> Int {
    _sanityCheck(_guts.isForeign)
    return __distance(from: i, to: j)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignCount() -> Int {
    _sanityCheck(_guts.isForeign)
    return __distance(from: startIndex, to: endIndex)
  }

}
