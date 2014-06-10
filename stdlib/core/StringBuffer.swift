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
    usedEnd = .null()
    capacityAndElementShift = elementWidth - 1
  }

  init(
    usedEnd: UnsafePointer<RawByte>,
    byteCapacity: Int,
    elementWidth: Int
  ) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    _sanityCheck((byteCapacity & 0x1) == 0)
    self.usedEnd = usedEnd
    self.capacityAndElementShift = byteCapacity + (elementWidth - 1)
  }

  var usedEnd: UnsafePointer<RawByte>
  var capacityAndElementShift: Int
  var byteCapacity: Int {
    return capacityAndElementShift & ~0x1
  }
  var elementShift: Int {
    return capacityAndElementShift & 0x1
  }
}

// FIXME: Wanted this to be a subclass of
// HeapBuffer<_StringBufferIVars,UTF16.CodeUnit>, but
// <rdar://problem/15520519> (Can't call static method of derived
// class of generic class with dependent argument type) prevents it.
struct _StringBuffer {

  // Make this a buffer of UTF16 code units so that it's properly
  // aligned for them if that's what we store.
  typealias _Storage = HeapBuffer<_StringBufferIVars, UTF16.CodeUnit>

  @conversion
  func __conversion() -> _Storage {
    return _storage
  }

  init(_ storage: _Storage) {
    _storage = storage
  }

  init(capacity: Int, initialSize: Int, elementWidth: Int) {
    _sanityCheck(elementWidth == 1 || elementWidth == 2)
    let elementShift = elementWidth - 1

    // We need at least 1 extra byte if we're storing 8-bit elements,
    // because indexing will always grab 2 consecutive bytes at a
    // time.
    let capacityBump = 1 - elementShift

    // Used to round capacity up to nearest multiple of 16 bits, the
    // element size of our storage.
    let divRound = 1 - elementShift
    _storage = _Storage(_Storage.Storage.self,
      _StringBufferIVars(elementWidth),
      (capacity + capacityBump + divRound) >> divRound
    )
    self.usedEnd = start + (initialSize << elementShift)
    _storage.value.capacityAndElementShift
      = ((_storage._capacity() - capacityBump) << 1) + elementShift
  }

  init<
    Encoding: UnicodeCodec, Input: Collection
  where Input.GeneratorType.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input, minimumCapacity: Int = 0
  ) {
    // Determine how many UTF16 code units we'll need
    var inputStream = input.generate()
    var (utf16Count, isAscii) = UTF16.measure(encoding, input: inputStream)

    // Allocate storage
    self = _StringBuffer(
      capacity: max(utf16Count, minimumCapacity),
      initialSize: utf16Count,
      elementWidth: isAscii ? 1 : 2)

    if isAscii {
      var p = UnsafePointer<UTF8.CodeUnit>(start)
      transcode(encoding, UTF32.self, input.generate(), SinkOf {
          (p++).memory = UTF8.CodeUnit($0)
        })
    }
    else {
      var p = _storage.elementStorage
      transcode(encoding, UTF16.self, input.generate(), SinkOf {
          (p++).memory = $0
        })
    }
  }

  /// a pointer to the start of this buffer's data area
  var start: UnsafePointer<RawByte> {
    return UnsafePointer(_storage.elementStorage)
  }

  /// a past-the-end pointer for this buffer's stored data
  var usedEnd: UnsafePointer<RawByte> {
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
  var capacityEnd: UnsafePointer<RawByte> {
    return start + _storage.value.byteCapacity
  }

  /// The number of elements that can be stored in this buffer
  var capacity: Int {
    return _storage.value.byteCapacity >> elementShift
  }

  /// 1 if the buffer stores UTF16; 0 otherwise
  var elementShift: Int {
    return _storage.value.elementShift
  }

  /// the number of bytes per element
  var elementWidth: Int {
    return elementShift + 1
  }

  mutating func grow(oldUsedEnd: UnsafePointer<RawByte>,
                     newUsedCount: Int) -> Bool {
    if _slowPath(newUsedCount > capacity) {
      return false
    }

    let newUsedEnd = start + (newUsedCount << elementShift)

    if _fastPath(
      self._storage.isUniquelyReferenced()
    ) {
      usedEnd = newUsedEnd
      return true
    }

    // FIXME: this function is currently NOT THREADSAFE.  The test +
    // assignment below should be replaced by a CAS
    if usedEnd == oldUsedEnd {
      usedEnd = newUsedEnd
      return true
    }
    return false
  }

  @conversion
  func __conversion() -> AnyObject? {
    return _storage.storage ? .Some(_storage.storage!) : .None
  }

  var _storage: _Storage
}
