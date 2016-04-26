
class CTLine {
}
struct CTLineBoundsOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var excludeTypographicLeading: CTLineBoundsOptions { get }
  static var excludeTypographicShifts: CTLineBoundsOptions { get }
  static var useHangingPunctuation: CTLineBoundsOptions { get }
  static var useGlyphPathBounds: CTLineBoundsOptions { get }
  static var useOpticalBounds: CTLineBoundsOptions { get }
  @available(tvOS 8.0, *)
  static var includeLanguageExtents: CTLineBoundsOptions { get }
}
enum CTLineTruncationType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case start
  case end
  case middle
}
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetTypeID() -> CFTypeID
@available(tvOS 3.2, *)
@discardableResult
func CTLineCreateWithAttributedString(_ attrString: CFAttributedString) -> CTLine
@available(tvOS 3.2, *)
@discardableResult
func CTLineCreateTruncatedLine(_ line: CTLine, _ width: Double, _ truncationType: CTLineTruncationType, _ truncationToken: CTLine?) -> CTLine?
@available(tvOS 3.2, *)
@discardableResult
func CTLineCreateJustifiedLine(_ line: CTLine, _ justificationFactor: CGFloat, _ justificationWidth: Double) -> CTLine?
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetGlyphCount(_ line: CTLine) -> CFIndex
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetGlyphRuns(_ line: CTLine) -> CFArray
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetStringRange(_ line: CTLine) -> CFRange
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetPenOffsetForFlush(_ line: CTLine, _ flushFactor: CGFloat, _ flushWidth: Double) -> Double
@available(tvOS 3.2, *)
func CTLineDraw(_ line: CTLine, _ context: CGContext)
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetTypographicBounds(_ line: CTLine, _ ascent: UnsafeMutablePointer<CGFloat>?, _ descent: UnsafeMutablePointer<CGFloat>?, _ leading: UnsafeMutablePointer<CGFloat>?) -> Double
@available(tvOS 6.0, *)
@discardableResult
func CTLineGetBoundsWithOptions(_ line: CTLine, _ options: CTLineBoundsOptions) -> CGRect
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetTrailingWhitespaceWidth(_ line: CTLine) -> Double
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetImageBounds(_ line: CTLine, _ context: CGContext?) -> CGRect
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetStringIndexForPosition(_ line: CTLine, _ position: CGPoint) -> CFIndex
@available(tvOS 3.2, *)
@discardableResult
func CTLineGetOffsetForStringIndex(_ line: CTLine, _ charIndex: CFIndex, _ secondaryOffset: UnsafeMutablePointer<CGFloat>?) -> CGFloat
@available(tvOS 9.0, *)
func CTLineEnumerateCaretOffsets(_ line: CTLine, _ block: (Double, CFIndex, Bool, UnsafeMutablePointer<Bool>) -> Void)
