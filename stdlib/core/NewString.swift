import swift

@objc, @class_protocol
protocol CocoaString {
  /// \brief Produce a SwiftString that represents the given subrange
  /// of this String
  func swiftRepresentation(range: Range<Int>) -> SwiftString

  /// \brief Say how many UTF16 code units this String occupies
  func countUTF16CodeUnits() -> Int

  /// \brief Forwards to NSString's "copy" method, which is expected
  /// to return an NSString that nobody else can modify with maximal
  /// efficiency, which could mean nothing more than a "retain," or
  /// otherwise probably means creating a representation that owns a
  /// flat buffer of UTF16 code units
  func toImmutable() -> CocoaString
}

/// \brief A wrapper around Range<Int> that adds error checking that
/// can't be done in the generic Range template
struct CheckedIntRange {
  init(value: Range<Int>) {
    assert(value.startIndex() >= 0)
    assert(value.endIndex() >= value.startIndex())
    self._value = value
  }

  func contains(subRange: Range<Int>) -> Bool {
    return (_value.startIndex() >= subRange.startIndex())
        && (_value.endIndex() <= subRange.endIndex())
  }

  func isEmpty() -> Bool {
    return _value.startIndex() == _value.endIndex()
  }

  @conversion func __conversion() -> Range<Int> {
    return _value
  }

  var _value: Range<Int>
}

/// \brief A value type holding a subrange of a given CocoaString's
/// code units.
struct CocoaStringSlice {

  /// \brief Bridge from Cocoa
  init(source: CocoaString) {
    // Since the source is a reference and we are presenting value
    // semantics, we need to make sure our buffer is immutable or
    // uniquely-owned by us.
    self.buffer = source.toImmutable() 
    self.range = CheckedIntRange(Range(0, source.countUTF16CodeUnits()))
  }

  /// \brief Construct a slice of source bounded by subRange
  init(source: CocoaStringSlice, subRange: CheckedIntRange) {
    assert(source.range.contains(subRange))
    buffer = subRange.isEmpty() ? .None : source.buffer
    self.range = subRange
  }

  var buffer: CocoaString?
  var range: CheckedIntRange
}

struct SwiftString {
  var owner: Builtin.ObjectPointer
  var start: UnsafePointer<UInt16>
  var count: Int
}

struct NewString {
  init(source: SwiftString) {
    representation = .Swift(source)
  }

  enum Representation {
  case Cocoa(CocoaStringSlice)
  case Swift(SwiftString)
  }

  var representation: Representation
}

// FIXME: Move to Foundation.swift
import Foundation

@asmname="CFStringGetCharactersPtr" func CFStringGetCharactersPtr(_: NSString) -> UnsafePointer<UInt16>

// This conformance is used to decouple the core Swift string
// representation from Cocoa
extension NSString : CocoaString {
  func swiftRepresentation(range: Range<Int>) -> SwiftString {

    var startIndex = range.startIndex()
    var count = range.endIndex() - startIndex

    // See if there's already a flat UTF-16 representation
    var flatCodeUnits: UnsafePointer<UInt16> = CFStringGetCharactersPtr(self)

    if !flatCodeUnits.isNull() {
      return SwiftString(owner: Builtin.castToObjectPointer(self), 
        start: flatCodeUnits + startIndex, count: count)
    }

    // No existing flat buffer; create and fill our own
    typealias StringBuffer = HeapBuffer<(),UInt16>
    var buffer = StringBuffer.create((), count)

    self.getCharacters(buffer.elementStorage, range: NSRange(startIndex, count))

    return SwiftString(
      owner: Builtin.castToObjectPointer(buffer), 
      start: buffer.elementStorage, count: count)
  }

  func countUTF16CodeUnits() -> Int {
    return self.length()
  }

  func toImmutable() -> CocoaString {
    var it : id = self.copy()
    return (it as NSString)!
  }
}
