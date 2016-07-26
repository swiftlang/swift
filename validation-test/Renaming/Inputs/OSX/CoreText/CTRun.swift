
class CTRun {
}
struct CTRunStatus : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var rightToLeft: CTRunStatus { get }
  static var nonMonotonic: CTRunStatus { get }
  static var hasNonIdentityMatrix: CTRunStatus { get }
}
@available(OSX 10.5, *)
@discardableResult
func CTRunGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
@discardableResult
func CTRunGetGlyphCount(_ run: CTRun) -> CFIndex
@available(OSX 10.5, *)
@discardableResult
func CTRunGetAttributes(_ run: CTRun) -> CFDictionary
@available(OSX 10.5, *)
@discardableResult
func CTRunGetStatus(_ run: CTRun) -> CTRunStatus
@available(OSX 10.5, *)
@discardableResult
func CTRunGetGlyphsPtr(_ run: CTRun) -> UnsafePointer<CGGlyph>?
@available(OSX 10.5, *)
func CTRunGetGlyphs(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CGGlyph>!)
@available(OSX 10.5, *)
@discardableResult
func CTRunGetPositionsPtr(_ run: CTRun) -> UnsafePointer<CGPoint>?
@available(OSX 10.5, *)
func CTRunGetPositions(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CGPoint>!)
@available(OSX 10.5, *)
@discardableResult
func CTRunGetAdvancesPtr(_ run: CTRun) -> UnsafePointer<CGSize>?
@available(OSX 10.5, *)
func CTRunGetAdvances(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CGSize>!)
@available(OSX 10.5, *)
@discardableResult
func CTRunGetStringIndicesPtr(_ run: CTRun) -> UnsafePointer<CFIndex>?
@available(OSX 10.5, *)
func CTRunGetStringIndices(_ run: CTRun, _ range: CFRange, _ buffer: UnsafeMutablePointer<CFIndex>!)
@available(OSX 10.5, *)
@discardableResult
func CTRunGetStringRange(_ run: CTRun) -> CFRange
@available(OSX 10.5, *)
@discardableResult
func CTRunGetTypographicBounds(_ run: CTRun, _ range: CFRange, _ ascent: UnsafeMutablePointer<CGFloat>?, _ descent: UnsafeMutablePointer<CGFloat>?, _ leading: UnsafeMutablePointer<CGFloat>?) -> Double
@available(OSX 10.5, *)
@discardableResult
func CTRunGetImageBounds(_ run: CTRun, _ context: CGContext?, _ range: CFRange) -> CGRect
@available(OSX 10.5, *)
@discardableResult
func CTRunGetTextMatrix(_ run: CTRun) -> CGAffineTransform
@available(OSX 10.5, *)
func CTRunDraw(_ run: CTRun, _ context: CGContext, _ range: CFRange)
