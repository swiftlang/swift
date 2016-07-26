
protocol NSAlignmentFeedbackToken : NSObjectProtocol {
}
@available(OSX 10.11, *)
class NSAlignmentFeedbackFilter : NSObject {
  @discardableResult
  class func inputEventMask() -> NSEventMask
  func update(with event: NSEvent)
  func update(withPanRecognizer panRecognizer: NSPanGestureRecognizer)
  @discardableResult
  func alignmentFeedbackTokenForMovement(in view: NSView?, previousPoint previousPoint: NSPoint, alignedPoint alignedPoint: NSPoint, defaultPoint defaultPoint: NSPoint) -> NSAlignmentFeedbackToken?
  @discardableResult
  func alignmentFeedbackTokenForHorizontalMovement(in view: NSView?, previousX previousX: CGFloat, alignedX alignedX: CGFloat, defaultX defaultX: CGFloat) -> NSAlignmentFeedbackToken?
  @discardableResult
  func alignmentFeedbackTokenForVerticalMovement(in view: NSView?, previousY previousY: CGFloat, alignedY alignedY: CGFloat, defaultY defaultY: CGFloat) -> NSAlignmentFeedbackToken?
  func performFeedback(_ alignmentFeedbackTokens: [NSAlignmentFeedbackToken], performanceTime performanceTime: NSHapticFeedbackPerformanceTime)
}
