//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// TODO: describe
//
// HACK HACK HACK: For whatever reason, having this directly on String instead
// of _StringGuts avoids a cascade of ARC. Also note, we can have a global
// function that forwards, but that function **must not be on _StringGuts**,
// else ARC.
//
extension String {
  @inlinable
  @inline(__always)
  func _visit<Result>(
    range: (Range<Int>, performBoundsCheck: Bool)? = nil,
    ascii: /*@convention(thin)*/ (_UnmanagedString<UInt8>) -> Result,
    utf16: /*@convention(thin)*/ (_UnmanagedString<UInt16>) -> Result,
    opaque: /*@convention(thin)*/ (_UnmanagedOpaqueString) -> Result
  ) -> Result {
    if _slowPath(_guts._isOpaque) {
      return self._visitOpaque(
        range: range, ascii: ascii, utf16: utf16, opaque: opaque)
    }

    defer { _fixLifetime(self) }
    if _guts.isASCII {
      var view = _guts._unmanagedASCIIView
      if let (range, boundsCheck) = range {
        if boundsCheck {
          view._boundsCheck(offsetRange: range)
        }
        view = view[range]
      }
      return ascii(view)
    } else {
      var view = _guts._unmanagedUTF16View
      if let (range, boundsCheck) = range {
        if boundsCheck {
          view._boundsCheck(offsetRange: range)
        }
        view = view[range]
      }
      return utf16(view)
    }
  }

  @usableFromInline
  @_effects(readonly)
  @inline(never) // @_outlined
  func _visitOpaque<Result>(
    range: (Range<Int>, performBoundsCheck: Bool)? = nil,
    ascii: /*@convention(thin)*/ (_UnmanagedString<UInt8>) -> Result,
    utf16: /*@convention(thin)*/ (_UnmanagedString<UInt16>) -> Result,
    opaque: /*@convention(thin)*/ (_UnmanagedOpaqueString) -> Result
  ) -> Result {
    _sanityCheck(_guts._isOpaque)

    if _guts._isSmall {
      _sanityCheck(_guts._object._isSmallUTF8, "no other small forms yet")
      let small = _guts._smallUTF8String
      if small.isASCII {
        return small.withUnmanagedASCII { view in
          var view = view
          if let (range, boundsCheck) = range {
            if boundsCheck {
              view._boundsCheck(offsetRange: range)
            }
            view = view[range]
          }
          return ascii(view)
        }
      }
      return small.withUnmanagedUTF16 { view in
        var view = view
        if let (range, boundsCheck) = range {
          if boundsCheck {
            view._boundsCheck(offsetRange: range)
          }
          view = view[range]
        }
        return utf16(view)
      }
    }

    // TODO: But can it provide a pointer+length representation?
    defer { _fixLifetime(self) }
    var view = _guts._asOpaque()
    if let (range, boundsCheck) = range {
      if boundsCheck {
        view._boundsCheck(offsetRange: range)
      }
      view = view[range]
    }

    return opaque(view)
  }

  @inlinable
  @inline(__always)
  func _visit<T, Result>(
    range: (Range<Int>, performBoundsCheck: Bool)?,
    args x: T,
    ascii: /*@convention(thin)*/ (_UnmanagedString<UInt8>, T) -> Result,
    utf16: /*@convention(thin)*/ (_UnmanagedString<UInt16>, T) -> Result,
    opaque: /*@convention(thin)*/ (_UnmanagedOpaqueString, T) -> Result
  ) -> Result {
    if _slowPath(_guts._isOpaque) {
      return self._visitOpaque(
        range: range, args: x, ascii: ascii, utf16: utf16, opaque: opaque)
    }

    defer { _fixLifetime(self) }
    if _guts.isASCII {
      var view = _guts._unmanagedASCIIView
      if let (range, boundsCheck) = range {
        if boundsCheck {
          view._boundsCheck(offsetRange: range)
        }
        view = view[range]
      }
      return ascii(view, x)
    } else  {
      var view = _guts._unmanagedUTF16View
      if let (range, boundsCheck) = range {
        if boundsCheck {
          view._boundsCheck(offsetRange: range)
        }
        view = view[range]
      }
      return utf16(view, x)
    }
  }

  @usableFromInline // @opaque
  @_effects(readonly)
  @inline(never)
  func _visitOpaque<T, Result>(
    range: (Range<Int>, performBoundsCheck: Bool)?,
    args x: T,
    ascii: /*@convention(thin)*/ (_UnmanagedString<UInt8>, T) -> Result,
    utf16: /*@convention(thin)*/ (_UnmanagedString<UInt16>, T) -> Result,
    opaque: /*@convention(thin)*/ (_UnmanagedOpaqueString, T) -> Result
  ) -> Result {
    _sanityCheck(_guts._isOpaque)

    if _fastPath(_guts._isSmall) {
      _sanityCheck(_guts._object._isSmallUTF8, "no other small forms yet")
      let small = _guts._smallUTF8String
      if small.isASCII {
        return small.withUnmanagedASCII { view in
          var view = view
          if let (range, boundsCheck) = range {
            if boundsCheck {
              view._boundsCheck(offsetRange: range)
            }
            view = view[range]
          }
          return ascii(view, x)
        }
      }
      return small.withUnmanagedUTF16 { view in
        var view = view
        if let (range, boundsCheck) = range {
          if boundsCheck {
            view._boundsCheck(offsetRange: range)
          }
          view = view[range]
        }
        return utf16(view, x)
      }
    }

    // TODO: But can it provide a pointer+length representation?
    defer { _fixLifetime(self) }
    var view = _guts._asOpaque()
    if let (range, boundsCheck) = range {
      if boundsCheck {
        view._boundsCheck(offsetRange: range)
      }
      view = view[range]
    }

    return opaque(view, x)
  }
}

@inlinable
@inline(__always)
internal
func _visitGuts<Result>(
  _ guts: _StringGuts,
  range: (Range<Int>, performBoundsCheck: Bool)? = nil,
  ascii: /*@convention(thin)*/ (_UnmanagedString<UInt8>) -> Result,
  utf16: /*@convention(thin)*/ (_UnmanagedString<UInt16>) -> Result,
  opaque: /*@convention(thin)*/ (_UnmanagedOpaqueString) -> Result
) -> Result {
  return String(guts)._visit(
    range: range, ascii: ascii, utf16: utf16, opaque: opaque)
}

@inlinable
@inline(__always)
internal
func _visitGuts<T, Result>(
  _ guts: _StringGuts,
  range: (Range<Int>, performBoundsCheck: Bool)? = nil,
  args x: T,
  ascii: /*@convention(thin)*/ (_UnmanagedString<UInt8>, T) -> Result,
  utf16: /*@convention(thin)*/ (_UnmanagedString<UInt16>, T) -> Result,
  opaque: /*@convention(thin)*/ (_UnmanagedOpaqueString, T) -> Result
) -> Result {
  return String(guts)._visit(
    range: range, args: x, ascii: ascii, utf16: utf16, opaque: opaque)
}

