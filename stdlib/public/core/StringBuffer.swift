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

  // This stored property should be stored at offset zero.  We perform atomic
  // operations on it using _HeapBuffer's pointer.
  var usedEnd: UnsafeMutableRawPointer?

  var capacityAndElementShift: Int
  var byteCapacity: Int {
    get {
      return Int(extendingOrTruncating:
        UInt(extendingOrTruncating: capacityAndElementShift) &>> 1)
    }
    set {
      capacityAndElementShift = elementShift | (newValue &<< 1)
    }
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

  /// Creates an instance with sufficient space for `capacity` elements and a
  /// `NUL` terminator.
  ///
  /// - Postcondition: `usedCount == initialSize`, 
  ///
  /// - Requires: `initialSize` <= `capacity`
  /// - Requires: `elementWidth == 1 || elementWidth == 2`
  public init(capacity: Int, initialSize: Int, elementWidth: Int) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    _sanityCheck(initialSize <= capacity)
    let header = _StringBufferIVars(_elementWidth: elementWidth)
    if elementWidth == 1 {
      // 1 byte to ensure rounding up when dividing by 2 and 1 byte for NUL
      // termination.
      _storage = _Storage(HeapBufferStorage.self, header, (capacity + 2) &>> 1)
      
      let nulTerminatedUTF8Capacity = _storage.capacity &<< 1
      _storage.value.byteCapacity = nulTerminatedUTF8Capacity &- 1
      
      let p = start.bindMemory(
        to: UTF8.CodeUnit.self, capacity: nulTerminatedUTF8Capacity)
      p[initialSize] = 0
      
      self.usedEnd = UnsafeMutableRawPointer(p + initialSize)
    }
    else {
      _storage = _Storage(HeapBufferStorage.self, header, capacity + 1)

      let nulTerminatedUTF16Capacity = _storage.capacity
      _storage.value.byteCapacity = (nulTerminatedUTF16Capacity &- 1) &<< 1
      
      let p = start.bindMemory(
        to: UTF16.CodeUnit.self, capacity: nulTerminatedUTF16Capacity)
      p[initialSize] = 0
      
      self.usedEnd = UnsafeMutableRawPointer(p + initialSize)
    }
  }

  static func fromCodeUnits<Input : Sequence, Encoding : _UnicodeEncoding>(
    _ input: Input, encoding: Encoding.Type, repairIllFormedSequences: Bool,
    minimumCapacity: Int = 0
  ) -> (_StringBuffer?, hadError: Bool)
    where Input.Element == Encoding.CodeUnit {
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
    return (usedEnd - start) &>> elementShift
  }

  /// A past-the-end pointer for this buffer's available storage.
  var capacityEnd: UnsafeMutableRawPointer {
    return start + _storage.value.byteCapacity
  }

  /// The number of elements that can be stored in this buffer.
  public var capacity: Int {
    return _storage.value.byteCapacity &>> elementShift
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
    let offset = (r.lowerBound - UnsafeRawPointer(start)) &>> elementShift
    return cap + offset <= capacity
  }

  var _anyObject: AnyObject? {
    return _storage.storage
  }

  var _storage: _Storage
}
