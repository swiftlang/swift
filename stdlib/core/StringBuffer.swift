//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

struct _StringBufferIVars {
  init(_ elementWidth: Int) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    usedEnd = nil
    capacityAndElementShift = elementWidth - 1
  }

  init(
    usedEnd: UnsafeMutablePointer<RawByte>,
    byteCapacity: Int,
    elementWidth: Int
  ) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    _sanityCheck((byteCapacity & 0x1) == 0)
    self.usedEnd = usedEnd
    self.capacityAndElementShift = byteCapacity + (elementWidth - 1)
  }

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it using _HeapBuffer's pointer.
  var usedEnd: UnsafeMutablePointer<RawByte>

  var capacityAndElementShift: Int
  var byteCapacity: Int {
    return capacityAndElementShift & ~0x1
  }
  var elementShift: Int {
    return capacityAndElementShift & 0x1
  }
}

// FIXME: Wanted this to be a subclass of
// _HeapBuffer<_StringBufferIVars,UTF16.CodeUnit>, but
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
    let elementShift = elementWidth - 1

    // We need at least 1 extra byte if we're storing 8-bit elements,
    // because indexing will always grab 2 consecutive bytes at a
    // time.
    let capacityBump = 1 - elementShift

    // Used to round capacity up to nearest multiple of 16 bits, the
    // element size of our storage.
    let divRound = 1 - elementShift
    _storage = _Storage(
      HeapBufferStorage.self,
      _StringBufferIVars(elementWidth),
      (capacity + capacityBump + divRound) >> divRound
    )
    self.usedEnd = start + (initialSize << elementShift)
    _storage.value.capacityAndElementShift
      = ((_storage._capacity() - capacityBump) << 1) + elementShift
  }

  static func fromCodeUnits<
    Encoding : UnicodeCodecType, Input : CollectionType // SequenceType?
    where Input.Generator.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input, repairIllFormedSequences: Bool,
    minimumCapacity: Int = 0
  ) -> (_StringBuffer?, hadError: Bool) {
    // Determine how many UTF-16 code units we'll need
    var inputStream = input.generate()
    if let (utf16Count, isAscii) = UTF16.measure(encoding, input: inputStream,
        repairIllFormedSequences: repairIllFormedSequences) {

      // Allocate storage
      var result = _StringBuffer(
          capacity: max(utf16Count, minimumCapacity),
          initialSize: utf16Count,
          elementWidth: isAscii ? 1 : 2)

      if isAscii {
        var p = UnsafeMutablePointer<UTF8.CodeUnit>(result.start)
        var sink = SinkOf<UTF32.CodeUnit> {
            (p++).memory = UTF8.CodeUnit($0)
          }
        let hadError = transcode(
          encoding, UTF32.self, input.generate(), &sink,
          stopOnError: !repairIllFormedSequences)
        _sanityCheck(!hadError, "string can not be ASCII if there were decoding errors")
        return (result, hadError)
      }
      else {
        var p = result._storage.baseAddress
        var sink = SinkOf<UTF16.CodeUnit> {
            (p++).memory = $0
          }
        let hadError = transcode(
          encoding, UTF16.self, input.generate(), &sink,
          stopOnError: !repairIllFormedSequences)
        return (result, hadError)
      }
    } else {
      return (.None, true)
    }
  }

  /// a pointer to the start of this buffer's data area
  public var start: UnsafeMutablePointer<RawByte> {
    return UnsafeMutablePointer(_storage.baseAddress)
  }

  /// a past-the-end pointer for this buffer's stored data
  var usedEnd: UnsafeMutablePointer<RawByte> {
    get {
      return _storage.value.usedEnd
    }
    set(newValue) {
      _storage.value.usedEnd = newValue
    }
  }

  var usedCount: Int {
    return (usedEnd - start) >> elementShift
  }

  /// a past-the-end pointer for this buffer's available storage
  var capacityEnd: UnsafeMutablePointer<RawByte> {
    return start + _storage.value.byteCapacity
  }

  /// The number of elements that can be stored in this buffer
  public var capacity: Int {
    return _storage.value.byteCapacity >> elementShift
  }

  /// 1 if the buffer stores UTF-16; 0 otherwise
  var elementShift: Int {
    return _storage.value.elementShift
  }

  /// the number of bytes per element
  var elementWidth: Int {
    return elementShift + 1
  }

  // Return true iff we have the given capacity for the indicated
  // substring.  This is what we need to do so that users can call
  // reserveCapacity on String and subsequently use that capacity, in
  // two separate phases.  Operations with one-phase growth should use
  // "grow()," below.  
  func hasCapacity(
    cap: Int, forSubRange r: Range<UnsafePointer<RawByte>>
  ) -> Bool {
    // The substring to be grown could be pointing in the middle of this
    // _StringBuffer.  
    let offset = (r.startIndex - UnsafePointer(start)) >> elementShift
    return cap + offset <= capacity
  }

  
  /// Attempt to claim unused capacity in the buffer.
  ///
  /// Operation succeeds if there is sufficient capacity, and either:
  /// - the buffer is uniquely-refereced, or
  /// - `oldUsedEnd` points to the end of the currently used capacity.
  ///
  /// :param: subRange range of the substring that the caller tries
  ///   to extend.
  /// :param: newUsedCount the desired size of the substring.
  mutating func grow(
    subRange: Range<UnsafePointer<RawByte>>, var newUsedCount: Int
  ) -> Bool {
    // The substring to be grown could be pointing in the middle of this
    // _StringBuffer.  Adjust the size so that it covers the imaginary
    // substring from the start of the buffer to `oldUsedEnd`.
    newUsedCount += (subRange.startIndex - UnsafePointer(start)) >> elementShift

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
    // if usedEnd == subRange.endIndex {
    //  usedEnd = newUsedEnd
    //  return true
    // }
    let usedEndPhysicalPtr =
      UnsafeMutablePointer<UnsafeMutablePointer<RawByte>>(_storage._value)
    var expected = UnsafeMutablePointer<RawByte>(subRange.endIndex)
    if _stdlib_atomicCompareExchangeStrongPtr(
      object: usedEndPhysicalPtr, expected: &expected, desired: newUsedEnd) {
      return true
    }

    return false
  }

  var _anyObject: AnyObject? {
    return _storage.storage != nil ? .Some(_storage.storage!) : .None
  }

  var _storage: _Storage
}
