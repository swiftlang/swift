
struct NSAffineTransformStruct {
  var m11: CGFloat
  var m12: CGFloat
  var m21: CGFloat
  var m22: CGFloat
  var tX: CGFloat
  var tY: CGFloat
  init()
  init(m11 m11: CGFloat, m12 m12: CGFloat, m21 m21: CGFloat, m22 m22: CGFloat, tX tX: CGFloat, tY tY: CGFloat)
}
class NSAffineTransform : NSObject, NSCopying, NSSecureCoding {
  convenience init(transform transform: NSAffineTransform)
  func translateX(by deltaX: CGFloat, yBy deltaY: CGFloat)
  func rotate(byDegrees angle: CGFloat)
  func rotate(byRadians angle: CGFloat)
  func scale(by scale: CGFloat)
  func scaleX(by scaleX: CGFloat, yBy scaleY: CGFloat)
  func invert()
  func append(_ transform: NSAffineTransform)
  func prepend(_ transform: NSAffineTransform)
  @discardableResult
  func transform(_ aPoint: NSPoint) -> NSPoint
  @discardableResult
  func transform(_ aSize: NSSize) -> NSSize
  var transformStruct: NSAffineTransformStruct
}
