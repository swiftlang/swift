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

@_versioned
struct _StringBufferIVars {
  internal init(_elementWidth: Int) {
    _sanityCheck(_elementWidth == 1 || _elementWidth == 2)
    usedEnd = nil
    capacityAndElementShift = _elementWidth - 1
  }

  internal init(
    _usedEnd: UnsafeMutableRawPointer,
    byteCapacity: Int,
    elementWidth: Int
  ) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    _sanityCheck((byteCapacity & 0x1) == 0)
    self.usedEnd = _usedEnd
    self.capacityAndElementShift = byteCapacity + (elementWidth - 1)
  }

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it using _HeapBuffer's pointer.
  var usedEnd: UnsafeMutableRawPointer?

  var capacityAndElementShift: Int
  var byteCapacity: Int {
    return capacityAndElementShift & ~0x1
  }
  var elementShift: Int {
    return capacityAndElementShift & 0x1
  }
}

// FIXME: Wanted this to be a subclass of
// _HeapBuffer<_StringBufferIVars, UTF16.CodeUnit>, but
// <rdar://problem/15520519> (Can't call static method of derived
// class of generic class with dependent argument type) prevents it.
public struct _StringBuffer {

  // Make this a buffer of UTF-16 code units so that it's properly
  // aligned for them if that's what we store.
  typealias _Storage = _HeapBuffer<_StringBufferIVars, UTF16.CodeUnit>
  typealias HeapBufferStorage
    = _HeapBufferStorage<_StringBufferIVars, UTF16.CodeUnit>

  init(_ storage: _Storage) {
    _storage = storage
  }

  public init(capacity: Int, initialSize: Int, elementWidth: Int) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    _sanityCheck(initialSize <= capacity)
    // We don't check for elementWidth overflow and underflow because
    // elementWidth is known to be 1 or 2.
    let elementShift = elementWidth &- 1

    // We need at least 1 extra byte if we're storing 8-bit elements,
    // because indexing will always grab 2 consecutive bytes at a
    // time.
    let capacityBump = 1 &- elementShift

    // Used to round capacity up to nearest multiple of 16 bits, the
    // element size of our storage.
    let divRound = 1 &- elementShift
    _storage = _Storage(
      HeapBufferStorage.self,
      _StringBufferIVars(_elementWidth: elementWidth),
      (capacity + capacityBump + divRound) >> divRound
    )
    // This conditional branch should fold away during code gen.
    if elementShift == 0 {
      start.bindMemory(to: UTF8.CodeUnit.self, capacity: initialSize)
    }
    else {
      start.bindMemory(to: UTF16.CodeUnit.self, capacity: initialSize)
    }

    self.usedEnd = start + (initialSize << elementShift)
    _storage.value.capacityAndElementShift
      = ((_storage._capacity() - capacityBump) << 1) + elementShift
  }

  static func fromCodeUnits<Input, Encoding>(
    _ input: Input, encoding: Encoding.Type, repairIllFormedSequences: Bool,
    minimumCapacity: Int = 0
  ) -> (_StringBuffer?, hadError: Bool)
    where
    Input : Collection, // Sequence?
    Encoding : UnicodeCodec,
    Input.Iterator.Element == Encoding.CodeUnit {
    // Determine how many UTF-16 code units we'll need
    let inputStream = input.makeIterator()
    guard let (utf16Count, isAscii) = UTF16.transcodedLength(
        of: inputStream,
        decodedAs: encoding,
        repairingIllFormedSequences: repairIllFormedSequences) else {
      return (nil, true)
    }

    // Allocate storage
    let result = _StringBuffer(
        capacity: max(utf16Count, minimumCapacity),
        initialSize: utf16Count,
        elementWidth: isAscii ? 1 : 2)

    if isAscii {
      var p = result.start.assumingMemoryBound(to: UTF8.CodeUnit.self)
      let sink: (UTF32.CodeUnit) -> Void = {
        p.pointee = UTF8.CodeUnit($0)
        p += 1
      }
      let hadError = transcode(
        input.makeIterator(),
        from: encoding, to: UTF32.self,
        stoppingOnError: true,
        into: sink)
      _sanityCheck(!hadError, "string cannot be ASCII if there were decoding errors")
      return (result, hadError)
    }
    else {
      var p = result._storage.baseAddress
      let sink: (UTF16.CodeUnit) -> Void = {
        p.pointee = $0
        p += 1
      }
      let hadError = transcode(
        input.makeIterator(),
        from: encoding, to: UTF16.self,
        stoppingOnError: !repairIllFormedSequences,
        into: sink)
      return (result, hadError)
    }
  }

  /// A pointer to the start of this buffer's data area.
  public // @testable
  var start: UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(_storage.baseAddress)
  }

  /// A past-the-end pointer for this buffer's stored data.
  var usedEnd: UnsafeMutableRawPointer {
    get {
      return _storage.value.usedEnd!
    }
    set(newValue) {
      _storage.value.usedEnd = newValue
    }
  }

  var usedCount: Int {
    return (usedEnd - start) >> elementShift
  }

  /// A past-the-end pointer for this buffer's available storage.
  var capacityEnd: UnsafeMutableRawPointer {
    return start + _storage.value.byteCapacity
  }

  /// The number of elements that can be stored in this buffer.
  public var capacity: Int {
    return _storage.value.byteCapacity >> elementShift
  }

  /// 1 if the buffer stores UTF-16; 0 otherwise.
  var elementShift: Int {
    return _storage.value.elementShift
  }

  /// The number of bytes per element.
  var elementWidth: Int {
    return elementShift + 1
  }

  // Return `true` iff we have the given capacity for the indicated
  // substring.  This is what we need to do so that users can call
  // reserveCapacity on String and subsequently use that capacity, in
  // two separate phases.  Operations with one-phase growth should use
  // "grow()," below.
  func hasCapacity(
    _ cap: Int, forSubRange r: Range<UnsafeRawPointer>
  ) -> Bool {
    // The substring to be grown could be pointing in the middle of this
    // _StringBuffer.
    let offset = (r.lowerBound - UnsafeRawPointer(start)) >> elementShift
    return cap + offset <= capacity
  }


  /// Attempt to claim unused capacity in the buffer.
  ///
  /// Operation succeeds if there is sufficient capacity, and either:
  /// - the buffer is uniquely-referenced, or
  /// - `oldUsedEnd` points to the end of the currently used capacity.
  ///
  /// - parameter bounds: Range of the substring that the caller tries
  ///   to extend.
  /// - parameter newUsedCount: The desired size of the substring.
  @inline(__always)
  @discardableResult
  mutating func grow(
    oldBounds bounds: Range<UnsafeRawPointer>, newUsedCount: Int
  ) -> Bool {
    var newUsedCount = newUsedCount
    // The substring to be grown could be pointing in the middle of this
    // _StringBuffer.  Adjust the size so that it covers the imaginary
    // substring from the start of the buffer to `oldUsedEnd`.
    newUsedCount
      += (bounds.lowerBound - UnsafeRawPointer(start)) >> elementShift

    if _slowPath(newUsedCount > capacity) {
      return false
    }

    let newUsedEnd = start + (newUsedCount << elementShift)

    if _fastPath(self._storage.isUniquelyReferenced()) {
      usedEnd = newUsedEnd
      return true
    }

    // Optimization: even if the buffer is shared, but the substring we are
    // trying to grow is located at the end of the buffer, it can be grown in
    // place.  The operation should be implemented in a thread-safe way,
    // though.
    //
    // if usedEnd == bounds.upperBound {
    //  usedEnd = newUsedEnd
    //  return true
    // }

    // &StringBufferIVars.usedEnd
    let usedEndPhysicalPtr = UnsafeMutableRawPointer(_storage._value)
      .assumingMemoryBound(to: Optional<UnsafeRawPointer>.self)
    // Create a temp var to hold the exchanged `expected` value.
    var expected : UnsafeRawPointer? = bounds.upperBound
    if _stdlib_atomicCompareExchangeStrongPtr(
      object: usedEndPhysicalPtr, expected: &expected,
      desired: UnsafeRawPointer(newUsedEnd)) {
      return true
    }

    return false
  }

  var _anyObject: AnyObject? {
    return _storage.storage != nil ? _storage.storage! : nil
  }

  var _storage: _Storage
}
