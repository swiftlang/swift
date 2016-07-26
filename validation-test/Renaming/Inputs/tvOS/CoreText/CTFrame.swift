
class CTFrame {
}
@available(tvOS 3.2, *)
@discardableResult
func CTFrameGetTypeID() -> CFTypeID
enum CTFrameProgression : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case topToBottom
  case rightToLeft
  case leftToRight
}
@available(tvOS 3.2, *)
let kCTFrameProgressionAttributeName: CFString
enum CTFramePathFillRule : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case evenOdd
  case windingNumber
}
@available(tvOS 4.2, *)
let kCTFramePathFillRuleAttributeName: CFString
@available(tvOS 4.2, *)
let kCTFramePathWidthAttributeName: CFString
@available(tvOS 4.3, *)
let kCTFrameClippingPathsAttributeName: CFString
@available(tvOS 4.3, *)
let kCTFramePathClippingPathAttributeName: CFString
@available(tvOS 3.2, *)
@discardableResult
func CTFrameGetStringRange(_ frame: CTFrame) -> CFRange
@available(tvOS 3.2, *)
@discardableResult
func CTFrameGetVisibleStringRange(_ frame: CTFrame) -> CFRange
@available(tvOS 3.2, *)
@discardableResult
func CTFrameGetPath(_ frame: CTFrame) -> CGPath
@available(tvOS 3.2, *)
@discardableResult
func CTFrameGetFrameAttributes(_ frame: CTFrame) -> CFDictionary?
@available(tvOS 3.2, *)
@discardableResult
func CTFrameGetLines(_ frame: CTFrame) -> CFArray
@available(tvOS 3.2, *)
func CTFrameGetLineOrigins(_ frame: CTFrame, _ range: CFRange, _ origins: UnsafeMutablePointer<CGPoint>!)
@available(tvOS 3.2, *)
func CTFrameDraw(_ frame: CTFrame, _ context: CGContext)
