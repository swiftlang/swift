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

/// The core implementation of a highly-optimizable String that
/// can store both ASCII and UTF-16, and can wrap native Swift
/// _StringBuffer or NSString instances.
///
/// Usage note: when elements are 8 bits wide, this code may
/// dereference one past the end of the byte array that it owns, so
/// make sure that storage is allocated!  You want a null terminator
/// anyway, so it shouldn't be a burden.
//
// Implementation note: We try hard to avoid branches in this code, so
// for example we use integer math to avoid switching on the element
// size with the ternary operator.  This is also the cause of the
// extra element requirement for 8 bit elements.  See the
// implementation of subscript(Int) -> UTF16.CodeUnit below for details.
struct _StringCore {
  //===--------------------------------------------------------------------===//
  // Internals
  var _baseAddress: COpaquePointer
  var _countAndFlags: UWord
  var _owner: AnyObject?

  /// (private) create the implementation of a string from its component parts.
  init(
    baseAddress: COpaquePointer,
    _countAndFlags: UWord,
    owner: AnyObject?
  ) {
    self._baseAddress = baseAddress
    self._countAndFlags = _countAndFlags
    self._owner = owner
    _invariantCheck()
  }

  func _invariantCheck() {
    assert(count >= 0)
    
    if _baseAddress == .null() {
      assert(cocoaBuffer,
        "Only opaque cocoa strings may have a null base pointer")
      assert(elementWidth == 2,
        "Opaque cocoa strings should have an elementWidth of 2")
    }
    else if _baseAddress == _emptyStringBase {
      assert(count == 0, "Empty string storage with non-zero length")
      assert(!_owner, "String pointing at empty storage has owner")
    }
    else if let buffer = nativeBuffer {
      assert(elementWidth == buffer.elementWidth,
        "_StringCore elementWidth doesn't match its buffer's")
      assert(UnsafePointer(_baseAddress) >= buffer.start)
      assert(UnsafePointer(_baseAddress) <= buffer.usedEnd)
      assert(UnsafePointer(_pointerToNth(count)) <= buffer.usedEnd)
    }
  }

  /// Bitmask for the count part of _countAndFlags
  var _countMask: UWord {
    return UWord.max >> 2
  }
  
  /// Bitmask for the flags part of _countAndFlags
  var _flagMask: UWord {
    return ~_countMask
  }

  /// Value by which to multiply a 2nd byte fetched in order to
  /// assemble a UTF16 code unit from our contiguous storage.  If we
  /// store ASCII, this will be zero.  Otherwise, it will be 0x100
  var _highByteMultiplier: UTF16.CodeUnit {
    return UTF16.CodeUnit(elementShift) << 8
  }

  /// Return a pointer to the Nth element of contiguous
  /// storage.  Caveats: The string must have contiguous storage; the
  /// element may be 1 or 2 bytes wide, depending on elementWidth; the
  /// result may be null if the string is empty.
  func _pointerToNth(n: Int) -> COpaquePointer {
    assert(hasContiguousStorage && n >= 0 && n <= count)
    return COpaquePointer(
      UnsafePointer<RawByte>(_baseAddress) + (n << elementShift))
  }

  static func _copyElements(
    srcStart: COpaquePointer, srcElementWidth: Int,
    dstStart: COpaquePointer, dstElementWidth: Int,
    count: Int
  ) {
    // Copy the old stuff into the new storage
    if _fastPath(srcElementWidth == dstElementWidth) {
      // No change in storage width; we can use memcpy
      c_memcpy(
        dest: UnsafePointer(dstStart),
        src: UnsafePointer(srcStart),
        size: UInt(count << (srcElementWidth - 1)))
    }
    else if (srcElementWidth < dstElementWidth) {
      // Widening ASCII to UTF16; we need to copy the bytes manually
      var dest = UnsafePointer<UTF16.CodeUnit>(dstStart)
      var src = UnsafePointer<UTF8.CodeUnit>(srcStart)
      let srcEnd = src + count
      while (src != srcEnd) {
        dest++.set(UTF16.CodeUnit(src++.get()))
      }
    }
    else {
      // Narrowing UTF16 to ASCII; we need to copy the bytes manually
      var dest = UnsafePointer<UTF8.CodeUnit>(dstStart)
      var src = UnsafePointer<UTF16.CodeUnit>(srcStart)
      let srcEnd = src + count
      while (src != srcEnd) {
        dest++.set(UTF8.CodeUnit(src++.get()))
      }
    }
  }
  
  //===--------------------------------------------------------------------===//
  // Initialization
  init(
    baseAddress: COpaquePointer,
    count: Int,
    elementShift: Int,
    hasCocoaBuffer: Bool,
    owner: AnyObject?
  ) {
    assert(elementShift == 0 || elementShift == 1)
    self._baseAddress = baseAddress
    
    self._countAndFlags
      = (UWord(elementShift) << UWord(sizeof(UWord.self) * 8 - 1))
      | ((hasCocoaBuffer ? 1 : 0) << UWord(sizeof(UWord.self) * 8 - 2)) 
      | UWord(count)
    
    self._owner = owner
    assert(UWord(count) & _flagMask == 0, "String too long to represent")
    _invariantCheck()
  }

  /// Create a _StringCore that covers the entire length of the _StringBuffer.
  init(_ buffer: _StringBuffer) {
    self = _StringCore(
      baseAddress: COpaquePointer(buffer.start),
      count: buffer.usedCount,
      elementShift: buffer.elementShift,
      hasCocoaBuffer: false,
      owner: buffer
    )
  }
  
  /// Create the implementation of an empty string.
  /// NOTE: there is no null terminator in an empty string!
  init() {
    self._baseAddress = _emptyStringBase
    self._countAndFlags = 0
    self._owner = .None
    _invariantCheck()
  }

  //===--------------------------------------------------------------------===//
  // Properties
  
  /// The number of elements stored
  var count: Int {
    get {
      return Int(_countAndFlags & _countMask)
    }
    set(newValue) {
      assert(UWord(newValue) & _flagMask == 0)
      _countAndFlags = (_countAndFlags & _flagMask) | UWord(newValue)
    }
  }

  /// left shift amount to apply to an offset N so that when
  /// added to a UnsafePointer<RawByte>, it traverses N elements
  var elementShift: Int {
    return Int(_countAndFlags >> UWord(sizeof(UWord.self) * 8 - 1))
  }
  
  /// the number of bytes per element
  var elementWidth: Int {
    return elementShift + 1
  }

  var hasContiguousStorage: Bool {
    return _fastPath(_baseAddress != .null())
  }

  /// are we using an NSString for storage?
  var hasCocoaBuffer: Bool {
    return Word((_countAndFlags << 1).value) < 0
  }

  var startASCII: UnsafePointer<UTF8.CodeUnit> {
    assert(elementWidth == 1, "String does not contain contiguous ASCII")
    return UnsafePointer(_baseAddress)
  }

  var startUTF16: UnsafePointer<UTF16.CodeUnit> {
    assert(
      count == 0 || elementWidth == 2,
      "String does not contain contiguous UTF16")
    return UnsafePointer(_baseAddress)
  }

  /// the native _StringBuffer, if any, or .None.
  var nativeBuffer: _StringBuffer? {
    if !hasCocoaBuffer {
      return _owner.map {
        reinterpretCast($0) as _StringBuffer
      }
    }
    return nil
  }

  /// the Cocoa String buffer, if any, or .None.
  var cocoaBuffer: _CocoaString? {
    if hasCocoaBuffer {
      return _owner.map {
        reinterpretCast($0) as _CocoaString
      }
    }
    return nil
  }
  
  //===--------------------------------------------------------------------===//
  // slicing
  
  /// Return the given sub-_StringCore
  subscript(subRange: Range<Int>) -> _StringCore {
    
    assert(subRange.startIndex >= 0)
    assert(subRange.endIndex <= count)

    let newCount = subRange.endIndex - subRange.startIndex
    assert(UWord(newCount) & _flagMask == 0)

    if hasContiguousStorage {
      return _StringCore(
        baseAddress: _pointerToNth(subRange.startIndex),
        _countAndFlags: (_countAndFlags & _flagMask) | UWord(newCount),
        owner: _owner)
    }
    return _cocoaStringSlice(target: self, subRange: subRange)
  }

  /// Get the Nth UTF16 Code Unit stored
  subscript(position: Int) -> UTF16.CodeUnit {
    assert(position >= 0)
    assert(position <= count)

    if (_baseAddress != .null()) {
      let p = UnsafePointer<UInt8>(_pointerToNth(position).value)
      // Always dereference two bytes, but when elements are 8 bits we
      // multiply the high byte by 0.
      return UTF16.CodeUnit(p.get())
           + UTF16.CodeUnit((p + 1).get()) * _highByteMultiplier
    }
    
    return _cocoaStringSubscript(target: self, position: position)
  }

  /// Write the string, in the given encoding, to output.
  func encode<
    Encoding: UnicodeCodec,
    Output: Sink
    where Encoding.CodeUnit == Output.Element
  >(encoding: Encoding.Type, output: Output) 
  {
    if _fastPath(_baseAddress != .null()) {
      if _fastPath(elementWidth == 1) {
        var out = output
        for x in UnsafeArray(
          start: UnsafePointer<UTF8.CodeUnit>(_baseAddress), length: count
        ) {
          Encoding.encode(UnicodeScalar(UInt32(x)), output: &out)
        }
      }
      else {
        transcode(UTF16.self, encoding,
          UnsafeArray(
            start: UnsafePointer<UTF16.CodeUnit>(_baseAddress),
            length: count
          ),
          output
        )
      }
    }
    else if (hasCocoaBuffer) {
      _StringCore(
        _cocoaStringToContiguous(source: cocoaBuffer!, range: 0...count, 
                                 minimumCapacity: 0)
      ).encode(encoding, output: output)
    }
  }

  /// Attempt to claim unused capacity in the String's existing
  /// native buffer, if any.  Return zero and a pointer to the claimed
  /// storage if successful. Otherwise, returns a suggested new
  /// capacity and a null pointer.  
  ///
  /// Note: If successful, effectively appends garbage to the String
  /// until it has newSize UTF16 code units; you must immediately copy
  /// valid UTF16 into that storage.
  ///
  /// Note: if unsuccessful because of insufficient space in an
  /// existing buffer, the suggested new capacity will at least double
  /// the existing buffer's storage
  mutating func _claimCapacity(newSize: Int, 
                               minElementWidth: Int) -> (Int, COpaquePointer) {
    if _fastPath(nativeBuffer && elementWidth >= minElementWidth) {
      var buffer = nativeBuffer!

      // The buffer's "used" field must match this in order to be
      // grown.  Otherwise, some other String is using parts of
      // the buffer beyond our last byte.
      let matchUsed = _pointerToNth(count)

      // Attempt to claim unused capacity in the buffer
      if _fastPath(buffer.grow(
          UnsafePointer<RawByte>(matchUsed.value), newUsedCount: newSize)) {
        count = newSize
        return (0, matchUsed)
      }
      else if newSize > buffer.capacity {
        // Growth failed because of insufficient storage; double the size
        return (max(buffer.capacity * 2, newSize), .null())
      }
    }
    return (newSize, .null())
  }

  /// Ensure that this String references a _StringBuffer having
  /// a capacity of at least newSize elements of at least the given width.
  /// Effectively appends garbage to the String until it has newSize
  /// UTF16 code units.  Returns a pointer to the garbage code units;
  /// you must immediately copy valid data into that storage.
  mutating func _growBuffer(
    newSize: Int, minElementWidth: Int
  ) -> COpaquePointer {
    let (newCapacity, existingStorage)
      = _claimCapacity(newSize, minElementWidth: minElementWidth)

    if _fastPath(!existingStorage.isNull()) {
      return existingStorage
    }

    // Allocate storage.
    let newElementWidth =
      minElementWidth >= elementWidth
      ? minElementWidth
      : representableAsASCII() ? 1 : 2

    var newStorage = _StringBuffer(capacity: newCapacity, initialSize: newSize,
                                   elementWidth: newElementWidth)

    var oldCount = count
    if hasContiguousStorage {
      _StringCore._copyElements(
        _baseAddress, srcElementWidth: elementWidth,
        dstStart: COpaquePointer(newStorage.start), 
        dstElementWidth: newElementWidth, count: oldCount)
    }
    else {
      // Opaque cocoa buffers might not store ASCII, so assert that
      // we've allocated for 2-byte elements.
      // FIXME: can we get Cocoa to tell us quickly that an opaque
      // string is ASCII?  Do we care much about that edge case?
      assert(newStorage.elementShift == 1)
      _cocoaStringReadAll(source: cocoaBuffer!, 
                          destination: UnsafePointer(newStorage.start))
    }
    
    self = _StringCore(newStorage)
    return _pointerToNth(oldCount)
  }

  mutating func append(c: UnicodeScalar) {
    _invariantCheck()
     // How many bytes does it take to encode each UTF16 code unit of
     // c if ASCII storage is available?
    let minBytesPerCodeUnit = c.value <= 0x7f ? 1 : 2
    // How many UTF16 code units does it take to encode c?
    let utf16Width = c.value <= 0xFFFF ? 1 : 2
    
    let destination = _growBuffer(count + utf16Width, 
                                  minElementWidth: minBytesPerCodeUnit)

    if _fastPath(elementWidth == 1) {
      assert(
        _pointerToNth(count) 
        == COpaquePointer(UnsafePointer<RawByte>(destination) + 1))

      UnsafePointer<UTF8.CodeUnit>(destination).set(UTF8.CodeUnit(c.value))
    }
    else {
      let destination16 = UnsafePointer<UTF16.CodeUnit>(destination.value)
      if _fastPath(utf16Width == 1) {
        assert(_pointerToNth(count) == COpaquePointer(destination16 + 1))
        destination16.set(UTF16.CodeUnit(c.value))
      }
      else {
        assert(_pointerToNth(count) == COpaquePointer(destination16 + 2))
        destination16.set(UTF16.leadSurrogate(c))
        (destination16 + 1).set(UTF16.trailSurrogate(c))
      }
    }
    _invariantCheck()
  }

  mutating func append(rhs: _StringCore) {
    _invariantCheck()
    let minElementWidth
    = elementWidth >= rhs.elementWidth
      ? elementWidth
      : rhs.representableAsASCII() ? 1 : 2

    let destination = _growBuffer(count + rhs.count, 
                                  minElementWidth: minElementWidth)
    if _fastPath(rhs.hasContiguousStorage) {
      _StringCore._copyElements(
        rhs._baseAddress, srcElementWidth: rhs.elementWidth, 
        dstStart: destination, dstElementWidth:elementWidth, count: rhs.count)
    }
    else {
      assert(elementWidth == 2)
      _cocoaStringReadAll(source: rhs.cocoaBuffer!, 
                          destination: UnsafePointer(destination))
    }
    _invariantCheck()
  }

  /// Return true iff the contents of this string can be
  /// represented as pure ASCII. O(N) in the worst case
  func representableAsASCII() -> Bool {
    if _slowPath(!hasContiguousStorage) {
      return false
    }
    if _fastPath(elementWidth == 1) {
      return true
    }
    return !contains(
      UnsafeArray(start: UnsafePointer<UTF16.CodeUnit>(_baseAddress), 
                  length: count)
    ) { $0 > 0x7f }
  }
}

extension _StringCore : Collection {
  var startIndex: Int {
    return 0
  }
  
  var endIndex: Int {
    return count
  }
  
  func generate() -> IndexingGenerator<_StringCore> {
    return IndexingGenerator(self)
  }
}

extension _StringCore : Sliceable {}

// Used to support a tighter invariant: all strings with contiguous
// storage have a non-NULL base address.
var _emptyStringStorage: UInt32 = 0

var _emptyStringBase: COpaquePointer {
  return COpaquePointer(
    UnsafePointer<UInt16>(Builtin.addressof(&_emptyStringStorage)))
}

