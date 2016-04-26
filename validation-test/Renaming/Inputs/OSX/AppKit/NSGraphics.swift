
enum NSCompositingOperation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case compositeClear
  case compositeCopy
  case compositeSourceOver
  case compositeSourceIn
  case compositeSourceOut
  case compositeSourceAtop
  case compositeDestinationOver
  case compositeDestinationIn
  case compositeDestinationOut
  case compositeDestinationAtop
  case compositeXOR
  case compositePlusDarker
  case compositePlusLighter
  @available(OSX 10.10, *)
  case compositeMultiply
  @available(OSX 10.10, *)
  case compositeScreen
  @available(OSX 10.10, *)
  case compositeOverlay
  @available(OSX 10.10, *)
  case compositeDarken
  @available(OSX 10.10, *)
  case compositeLighten
  @available(OSX 10.10, *)
  case compositeColorDodge
  @available(OSX 10.10, *)
  case compositeColorBurn
  @available(OSX 10.10, *)
  case compositeSoftLight
  @available(OSX 10.10, *)
  case compositeHardLight
  @available(OSX 10.10, *)
  case compositeDifference
  @available(OSX 10.10, *)
  case compositeExclusion
  @available(OSX 10.10, *)
  case compositeHue
  @available(OSX 10.10, *)
  case compositeSaturation
  @available(OSX 10.10, *)
  case compositeColor
  @available(OSX 10.10, *)
  case compositeLuminosity
}
enum NSBackingStoreType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case retained
  case nonretained
  case buffered
}
enum NSWindowOrderingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case above
  case below
  case out
}
enum NSFocusRingPlacement : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case only
  case below
  case above
}
enum NSFocusRingType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case none
  case exterior
}
@available(OSX 10.5, *)
enum NSColorRenderingIntent : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case absoluteColorimetric
  case relativeColorimetric
  case perceptual
  case saturation
}
let NSCalibratedWhiteColorSpace: String
let NSCalibratedRGBColorSpace: String
let NSDeviceWhiteColorSpace: String
let NSDeviceRGBColorSpace: String
let NSDeviceCMYKColorSpace: String
let NSNamedColorSpace: String
let NSPatternColorSpace: String
let NSCustomColorSpace: String
var NSWindowDepthTwentyfourBitRGB: Int { get }
var NSWindowDepthSixtyfourBitRGB: Int { get }
var NSWindowDepthOnehundredtwentyeightBitRGB: Int { get }
typealias NSWindowDepth = Int32
@discardableResult
func NSBestDepth(_ colorSpace: String, _ bps: Int, _ bpp: Int, _ planar: Bool, _ exactMatch: UnsafeMutablePointer<ObjCBool>?) -> NSWindowDepth
@discardableResult
func NSPlanarFromDepth(_ depth: NSWindowDepth) -> Bool
@discardableResult
func NSColorSpaceFromDepth(_ depth: NSWindowDepth) -> String?
@discardableResult
func NSBitsPerSampleFromDepth(_ depth: NSWindowDepth) -> Int
@discardableResult
func NSBitsPerPixelFromDepth(_ depth: NSWindowDepth) -> Int
@discardableResult
func NSNumberOfColorComponents(_ colorSpaceName: String) -> Int
@discardableResult
func NSAvailableWindowDepths() -> UnsafePointer<NSWindowDepth>
let NSWhite: CGFloat
let NSLightGray: CGFloat
let NSDarkGray: CGFloat
let NSBlack: CGFloat
let NSDeviceResolution: String
let NSDeviceColorSpaceName: String
let NSDeviceBitsPerSample: String
let NSDeviceIsScreen: String
let NSDeviceIsPrinter: String
let NSDeviceSize: String
func NSRectFill(_ aRect: NSRect)
func NSRectFillList(_ rects: UnsafePointer<NSRect>, _ count: Int)
func NSRectFillListWithGrays(_ rects: UnsafePointer<NSRect>, _ grays: UnsafePointer<CGFloat>, _ num: Int)
func NSRectFillListWithColors(_ rects: UnsafePointer<NSRect>, _ colors: UnsafePointer<NSColor>, _ num: Int)
func NSRectFillUsingOperation(_ aRect: NSRect, _ op: NSCompositingOperation)
func NSRectFillListUsingOperation(_ rects: UnsafePointer<NSRect>, _ count: Int, _ op: NSCompositingOperation)
func NSRectFillListWithColorsUsingOperation(_ rects: UnsafePointer<NSRect>, _ colors: UnsafePointer<NSColor>, _ num: Int, _ op: NSCompositingOperation)
func NSFrameRect(_ aRect: NSRect)
func NSFrameRectWithWidth(_ aRect: NSRect, _ frameWidth: CGFloat)
func NSFrameRectWithWidthUsingOperation(_ aRect: NSRect, _ frameWidth: CGFloat, _ op: NSCompositingOperation)
func NSRectClip(_ aRect: NSRect)
func NSRectClipList(_ rects: UnsafePointer<NSRect>, _ count: Int)
@discardableResult
func NSDrawTiledRects(_ boundsRect: NSRect, _ clipRect: NSRect, _ sides: UnsafePointer<NSRectEdge>, _ grays: UnsafePointer<CGFloat>, _ count: Int) -> NSRect
func NSDrawGrayBezel(_ aRect: NSRect, _ clipRect: NSRect)
func NSDrawGroove(_ aRect: NSRect, _ clipRect: NSRect)
func NSDrawWhiteBezel(_ aRect: NSRect, _ clipRect: NSRect)
func NSDrawButton(_ aRect: NSRect, _ clipRect: NSRect)
func NSEraseRect(_ aRect: NSRect)
@discardableResult
func NSReadPixel(_ passedPoint: NSPoint) -> NSColor?
func NSDrawBitmap(_ rect: NSRect, _ width: Int, _ height: Int, _ bps: Int, _ spp: Int, _ bpp: Int, _ bpr: Int, _ isPlanar: Bool, _ hasAlpha: Bool, _ colorSpaceName: String, _ data: UnsafePointer<UnsafePointer<UInt8>?>!)
func NSBeep()
@discardableResult
func NSGetWindowServerMemory(_ context: Int, _ virtualMemory: UnsafeMutablePointer<Int>, _ windowBackingMemory: UnsafeMutablePointer<Int>, _ windowDumpString: AutoreleasingUnsafeMutablePointer<NSString>) -> Int
@discardableResult
func NSDrawColorTiledRects(_ boundsRect: NSRect, _ clipRect: NSRect, _ sides: UnsafePointer<NSRectEdge>, _ colors: AutoreleasingUnsafeMutablePointer<NSColor>, _ count: Int) -> NSRect
func NSDrawDarkBezel(_ aRect: NSRect, _ clipRect: NSRect)
func NSDrawLightBezel(_ aRect: NSRect, _ clipRect: NSRect)
func NSDottedFrameRect(_ aRect: NSRect)
func NSDrawWindowBackground(_ aRect: NSRect)
func NSSetFocusRingStyle(_ placement: NSFocusRingPlacement)
func NSDisableScreenUpdates()
func NSEnableScreenUpdates()
enum NSAnimationEffect : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case disappearingItemDefault
  case poof
}
func NSShowAnimationEffect(_ animationEffect: NSAnimationEffect, _ centerLocation: NSPoint, _ size: NSSize, _ animationDelegate: AnyObject?, _ didEndSelector: Selector?, _ contextInfo: UnsafeMutablePointer<Void>?)
@available(OSX, introduced: 10.0, deprecated: 10.10)
func NSCopyBits(_ srcGState: Int, _ srcRect: NSRect, _ destPoint: NSPoint)
