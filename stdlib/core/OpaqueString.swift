/// \brief After Foundation is loaded, contains a function that
/// produces a ContiguousString from a given subrange of a source
/// _CocoaString, having the given minimum capacity.
var __swift_cocoaStringToContiguousString : (
  source: _CocoaString, range: Range<Int>, minimumCapacity: Int
) -> ContiguousString = __swift_cocoaStringToContiguousStringDefault

func __swift_cocoaStringToContiguousStringDefault(source:
  _CocoaString, range: Range<Int>, minimumCapacity: Int) -> ContiguousString
{
  alwaysTrap("Swift <==> Cocoa string bridge not initialized")
}


/// \brief After Foundation is loaded, contains a function that reads
/// the entire contents of a _CocoaString into contiguous storage of
/// sufficient capacity.
var __swift_cocoaStringReadAll : (
  source: _CocoaString, destination: UnsafePointer<UTF16.CodeUnit>
) -> Void = __swift_cocoaStringReadAllDefault

func __swift_cocoaStringReadAllDefault(source: _CocoaString, destination: UnsafePointer<UTF16.CodeUnit>) {
  alwaysTrap("Swift <==> Cocoa string bridge not initialized")
}

//===----------------------------------------------------------------------===//

/// \brief A value type holding a subrange of a _CocoaString
struct OpaqueString {
  init(source: _CocoaString, subRange: Range<Int>) {
    self.buffer = source
    self.range = subRange
  }
  
  /// \brief Construct a slice of source bounded by subRange
  init(source: OpaqueString, subRange: Range<Int>) {
    assert(!subRange.isEmpty(), "For this case we expect to construct a contiguous slice")
    buffer = source.buffer
    self.range = subRange
  }

  subscript(bounds: Range<Int>) -> OpaqueString {
    return OpaqueString(self, bounds)
  }

  /// \brief Convert this whole slice to a contiguous representation
  /// with storage for at least minimumCapacity UTF16 code units.
  /// For efficiency, consider dispatching to Cocoa functions that
  /// accept a range argument, rather than calling this function and
  /// operating on that slice.
  func contiguousString(minimumCapacity: Int = 0) -> ContiguousString {
    return contiguousString(range, minimumCapacity)
  }
  
  /// \brief Convert a subRange of this slice to a contiguous
  /// representation.  with storage for at least minimumCapacity UTF16
  /// code units. For efficiency, consider dispatching to Cocoa
  /// functions rather than calling this function and operating on
  /// that slice.
  func contiguousString(subRange: Range<Int>, minimumCapacity: Int = 0) -> ContiguousString {
    return __swift_cocoaStringToContiguousString(buffer, subRange, minimumCapacity)
  }
  
  var buffer: _CocoaString
  var range: Range<Int>
}
