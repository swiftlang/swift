/// \brief The core implementation of a highly-optimizable String that
/// can store both ASCII and UTF-16, and can wrap native Swift
/// StringBuffer or NSString instances.
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
struct StringCore {
  var baseAddress: COpaquePointer
  var _countAndFlags: UWord
  var owner: AnyObject?

  /// \brief (private) create the implementation of a string from its
  /// component parts.
  init(
    baseAddress: COpaquePointer,
    _countAndFlags: UWord,
    owner: AnyObject?
  ) {
    self.baseAddress = baseAddress
    self._countAndFlags = _countAndFlags
    self.owner = owner
  }

  init(
    baseAddress: COpaquePointer,
    count: Int,
    elementShift: Int,
    hasOpaqueBuffer: Bool,
    owner: AnyObject?
  ) {
    assert(elementShift == 0 || elementShift == 1)
    self.baseAddress = baseAddress
    self._countAndFlags = (UWord(elementShift) << (UWord.bitSize() - 1))
                     | ((hasOpaqueBuffer ? 1 : 0) << (UWord.bitSize() - 2))
                     | UWord(count)
    self.owner = owner
    assert(UWord(count) & _flagMask == 0)
  }

  /// \brief Create the implementation of an empty string.  NOTE:
  /// there is no null terminator in an empty string!
  init() {
    self.baseAddress = COpaquePointer()
    self._countAndFlags = 0
    self.owner = .None
  }

  /// \brief bitmask for the count part of _countAndFlags
  var _countMask: UWord {
    return UWord.max >> 2
  }
  
  /// \brief bitmask for the flags part of _countAndFlags
  var _flagMask: UWord {
    return ~_countMask
  }
  
  /// \brief the number of elements stored
  var count: Int {
    return Int(_countAndFlags & _countMask)
  set(newValue):
    assert(UWord(newValue) & _flagMask == 0)
    _countAndFlags = (_countAndFlags & _flagMask) | UWord(newValue)
  }

  /// \brief left shift amount to apply to an offset N so that when
  /// added to a UnsafePointer<RawByte>, it traverses N elements
  var elementShift: Int {
    return Int(_countAndFlags >> (UWord.bitSize() - 1))
  }
  
  /// \brief the number of bytes per element
  var elementSize: Int {
    return elementShift + 1
  }

  /// \brief are we using an opaque NSString for storage?
  var hasOpaqueBuffer: Bool {
    return Word((_countAndFlags << 1).value) < 0
  }

  /// \brief Return the given sub-StringCore
  subscript(subRange: Range<Int>) -> StringCore {
    
    assert(subRange.startIndex() >= 0)
    assert(subRange.endIndex() <= count)

    let newCount = subRange.endIndex() - subRange.startIndex()
    assert(UWord(newCount) & _flagMask == 0)

    if (!hasOpaqueBuffer) {
      return StringCore(
        baseAddress: COpaquePointer(
          UnsafePointer<RawByte>(baseAddress.value)
          + (subRange.startIndex() << elementShift)
        ),
        _countAndFlags: (_countAndFlags & _flagMask) | UWord(newCount),
        owner: owner)
    }
    return _opaqueSlice(subRange, newCount)
  }

  /// \brief helper function for the slicing subscript above.
  /// Implements the slow path
  func _opaqueSlice(subRange: Range<Int>, newCount: Int) -> StringCore {
    alwaysTrap("IMPLEMENT ME")
  }

  /// \brief value by which to multiply a 2nd byte fetched in order to
  /// assemble a UTF16 code unit from our contiguous storage.  If we
  /// store ASCII, this will be zero.  Otherwise, it will be 0x100
  var _highByteMultiplier: UTF16.CodeUnit {
    return UTF16.CodeUnit(elementShift) << 8
  }
  
  /// \brief Get the Nth UTF16 Code Unit stored
  subscript(position: Int) -> UTF16.CodeUnit {
    assert(position >= 0)
    assert(position <= count)

    if (!hasOpaqueBuffer) {
      let p = UnsafePointer<UInt8>(baseAddress.value) + (position << elementShift)
      // Always dereference two bytes, but when elements are 8 bits we
      // multiply the high byte by 0.
      return UTF16.CodeUnit(p.get())
           + UTF16.CodeUnit((p + 1).get()) * _highByteMultiplier
    }
    
    return _opaqueSubscript(position)
  }

  /// \brief helper function for ordinary subscript above.  Implements
  /// the slow path
  func _opaqueSubscript(position: Int) -> UTF16.CodeUnit {
    alwaysTrap("IMPLEMENT ME")
  }
}
