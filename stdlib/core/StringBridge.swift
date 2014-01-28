/// \brief Loading Foundation initializes these function variables
/// with useful values

func _cocoaStringBridgeNotInitialized<A,R>() -> (A)->R {
  // FIXME: returning a closure as a workaround for
  // <rdar://problem/15921334>
  return {
    (args:A) -> R in
    fatal("Swift <==> Cocoa string bridge not initialized")
    return (.None as R?)!
  }
}

/// \brief produces a ContiguousString from a given subrange of a source
/// _CocoaString, having the given minimum capacity.
var __swift_cocoaStringToContiguousString : (
  source: _CocoaString, range: Range<Int>, minimumCapacity: Int
) -> ContiguousString = _cocoaStringBridgeNotInitialized()

/// \brief reads the entire contents of a _CocoaString into contiguous
/// storage of sufficient capacity.
var __swift_cocoaStringReadAll : (
  source: _CocoaString, destination: UnsafePointer<UTF16.CodeUnit>
) -> Void = _cocoaStringBridgeNotInitialized()

// FIXME: This will probably go away but is needed at least
// temporarily as a bridge to StringCore
var __swift_cocoaStringLength: (
  source: _CocoaString
) -> Int = _cocoaStringBridgeNotInitialized()

var _appendCocoaString : (
  target: @inout String, rhs: String
) -> Void
// FIXME: Explicit default value is a workaround for <rdar://problem/15921520> 
= { x,y in fatal("Swift <==> Cocoa string bridge not initialized") }

var _sliceCocoaString : (
  target: StringCore, subRange: Range<Int>
) -> StringCore = _cocoaStringBridgeNotInitialized()

var _indexCocoaString : (
  target: StringCore, position: Int
) -> UTF16.CodeUnit = _cocoaStringBridgeNotInitialized()

