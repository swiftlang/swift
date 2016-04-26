
class CTRun {
}
struct CTRunStatus : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var rightToLeft: CTRunStatus { get }
  static var nonMonotonic: CTRunStatus { get }
  static var hasNonIdentityMatrix: CTRunStatus { get }
}
@available(iOS 3.2, *)
@discardableResult
func CTRunGetTypeID() -> CFTypeID
@available(iOS 3.2, *)
@discardableResult
func CTRunGetGlyphCount(_ run: CTRun) -> CFIndex
@available(iOS 3.2, *)
@discardableResult
func CTRunGetAttributes(_ run: CTRun) -> CFDictionary
@available(iOS 3.2, *)
@discardableResult
func CTRunGetStatus(_ run: CTRun) -> CTRunStatus
@available(iOS 3.2, *)
@discardableResult
func CTRunGetGlyphsPtr(_ run: CTRun) -> UnsafePointer<CGGlyph>?
@available(iOS 3.2, *)
func CTRunGetGlyphs(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CGGlyph>!)
@available(iOS 3.2, *)
@discardableResult
func CTRunGetPositionsPtr(_ run: CTRun) -> UnsafePointer<CGPoint>?
@available(iOS 3.2, *)
func CTRunGetPositions(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CGPoint>!)
@available(iOS 3.2, *)
@discardableResult
func CTRunGetAdvancesPtr(_ run: CTRun) -> UnsafePointer<CGSize>?
@available(iOS 3.2, *)
func CTRunGetAdvances(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CGSize>!)
@available(iOS 3.2, *)
@discardableResult
func CTRunGetStringIndicesPtr(_ run: CTRun) -> UnsafePointer<CFIndex>?
@available(iOS 3.2, *)
func CTRunGetStringIndices(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CFIndex>!)
@available(iOS 3.2, *)
@discardableResult
func CTRunGetStringRange(_ run: CTRun) -> CFRange
@available(iOS 3.2, *)
@discardableResult
func CTRunGetTypographicBounds(_ run: CTRun, _ range: CFRange, _ ascent: UnsafeMutablePointer<CGFloat>?, _ descent: UnsafeMutablePointer<CGFloat>?, _ leading: UnsafeMutablePointer<CGFloat>?) -> Double
@available(iOS 3.2, *)
@discardableResult
func CTRunGetImageBounds(_ run: CTRun, _ context: CGContext?, _ range: CFRange) -> CGRect
@available(iOS 3.2, *)
@discardableResult
func CTRunGetTextMatrix(_ run: CTRun) -> CGAffineTransform
@available(iOS 3.2, *)
func CTRunDraw(_ run: CTRun, _ context: CGContext, _ range: CFRange)
