
class ScreenSaverView : NSView {
  @discardableResult
  class func backingStoreType() -> NSBackingStoreType
  @discardableResult
  class func performGammaFade() -> Bool
  init?(frame frame: NSRect, isPreview isPreview: Bool)
  var animationTimeInterval: NSTimeInterval
  func startAnimation()
  func stopAnimation()
  var isAnimating: Bool { get }
  func animateOneFrame()
  @discardableResult
  func hasConfigureSheet() -> Bool
  @discardableResult
  func configureSheet() -> NSWindow?
  var isPreview: Bool { get }
}
@discardableResult
func SSRandomIntBetween(_ a: Int32, _ b: Int32) -> Int32
@discardableResult
func SSRandomFloatBetween(_ a: CGFloat, _ b: CGFloat) -> CGFloat
@discardableResult
func SSRandomPointForSizeWithinRect(_ size: NSSize, _ rect: NSRect) -> NSPoint
@discardableResult
func SSCenteredRectInRect(_ innerRect: NSRect, _ outerRect: NSRect) -> NSRect
