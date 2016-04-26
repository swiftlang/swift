
@available(OSX 10.4, *)
class CIVector : NSObject, NSCopying, NSSecureCoding {
  init(values values: UnsafePointer<CGFloat>, count count: Int)
  convenience init(x x: CGFloat)
  convenience init(x x: CGFloat, y y: CGFloat)
  convenience init(x x: CGFloat, y y: CGFloat, z z: CGFloat)
  convenience init(x x: CGFloat, y y: CGFloat, z z: CGFloat, w w: CGFloat)
  @available(OSX 10.9, *)
  convenience init(cgPoint p: CGPoint)
  @available(OSX 10.9, *)
  convenience init(cgRect r: CGRect)
  @available(OSX 10.9, *)
  convenience init(cgAffineTransform r: CGAffineTransform)
  convenience init(string representation: String)
  @discardableResult
  func value(at index: Int) -> CGFloat
  var count: Int { get }
  var x: CGFloat { get }
  var y: CGFloat { get }
  var z: CGFloat { get }
  var w: CGFloat { get }
  @available(OSX 10.9, *)
  var cgPointValue: CGPoint { get }
  @available(OSX 10.9, *)
  var cgRectValue: CGRect { get }
  @available(OSX 10.9, *)
  var cgAffineTransformValue: CGAffineTransform { get }
  var stringRepresentation: String { get }
}
