
class CGMutablePath {
}
class CGPath {
}
enum CGLineJoin : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case miter
  case round
  case bevel
}
enum CGLineCap : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case butt
  case round
  case square
}
extension CGPath {
  @available(tvOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(tvOS 2.0, *)
  init?(copy path: CGPath?)
  @available(tvOS 5.0, *)
  init?(copyByTransforming path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(tvOS 4.0, *)
  init(with rect: CGRect, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(tvOS 5.0, *)
  init(withEllipseIn rect: CGRect, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(tvOS 7.0, *)
  init(withRoundedRect rect: CGRect, cornerWidth cornerWidth: CGFloat, cornerHeight cornerHeight: CGFloat, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(tvOS 5.0, *)
  init?(copyByDashingPath path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?, phase phase: CGFloat, lengths lengths: UnsafePointer<CGFloat>?, count count: Int)
  @available(tvOS 5.0, *)
  init?(copyByStroking path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?, lineWidth lineWidth: CGFloat, lineCap lineCap: CGLineCap, lineJoin lineJoin: CGLineJoin, miterLimit miterLimit: CGFloat)
  @available(tvOS 2.0, *)
  @discardableResult
  func equalTo(_ path2: CGPath?) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func isEmpty() -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func isRect(_ rect: UnsafeMutablePointer<CGRect>?) -> Bool
  @available(tvOS 2.0, *)
  var currentPoint: CGPoint { get }
  @available(tvOS 2.0, *)
  var boundingBox: CGRect { get }
  @available(tvOS 4.0, *)
  var boundingBoxOfPath: CGRect { get }
  @available(tvOS 2.0, *)
  @discardableResult
  func containsPoint(_ m: UnsafePointer<CGAffineTransform>?, point point: CGPoint, eoFill eoFill: Bool) -> Bool
  @available(tvOS 2.0, *)
  func apply(info info: UnsafeMutablePointer<Void>?, function function: CGPathApplierFunction?)
}
extension CGMutablePath {
  @available(tvOS 2.0, *)
  init()
  @available(tvOS 2.0, *)
  init?(copy path: CGPath?)
  @available(tvOS 5.0, *)
  init?(copyByTransforming path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(tvOS 7.0, *)
  func addRoundedRect(_ transform: UnsafePointer<CGAffineTransform>?, rect rect: CGRect, cornerWidth cornerWidth: CGFloat, cornerHeight cornerHeight: CGFloat)
  @available(tvOS 2.0, *)
  func moveTo(_ m: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat)
  @available(tvOS 2.0, *)
  func addLineTo(_ m: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat)
  @available(tvOS 2.0, *)
  func addQuadCurve(_ m: UnsafePointer<CGAffineTransform>?, cpx cpx: CGFloat, cpy cpy: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(tvOS 2.0, *)
  func addCurve(_ m: UnsafePointer<CGAffineTransform>?, cp1x cp1x: CGFloat, cp1y cp1y: CGFloat, cp2x cp2x: CGFloat, cp2y cp2y: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(tvOS 2.0, *)
  func closeSubpath()
  @available(tvOS 2.0, *)
  func addRect(_ m: UnsafePointer<CGAffineTransform>?, rect rect: CGRect)
  @available(tvOS 2.0, *)
  func addRects(_ m: UnsafePointer<CGAffineTransform>?, rects rects: UnsafePointer<CGRect>?, count count: Int)
  @available(tvOS 2.0, *)
  func addLines(_ m: UnsafePointer<CGAffineTransform>?, between points: UnsafePointer<CGPoint>?, count count: Int)
  @available(tvOS 2.0, *)
  func addEllipseIn(_ m: UnsafePointer<CGAffineTransform>?, rect rect: CGRect)
  @available(tvOS 5.0, *)
  func addRelativeArc(matrix matrix: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat, radius radius: CGFloat, startAngle startAngle: CGFloat, delta delta: CGFloat)
  @available(tvOS 2.0, *)
  func addArc(_ m: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat, radius radius: CGFloat, startAngle startAngle: CGFloat, endAngle endAngle: CGFloat, clockwise clockwise: Bool)
  @available(tvOS 2.0, *)
  func addArc(_ m: UnsafePointer<CGAffineTransform>?, x1 x1: CGFloat, y1 y1: CGFloat, x2 x2: CGFloat, y2 y2: CGFloat, radius radius: CGFloat)
  @available(tvOS 2.0, *)
  func addPath(_ m: UnsafePointer<CGAffineTransform>?, path path2: CGPath?)
}
enum CGPathElementType : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case moveToPoint
  case addLineToPoint
  case addQuadCurveToPoint
  case addCurveToPoint
  case closeSubpath
}
struct CGPathElement {
  var type: CGPathElementType
  var points: UnsafeMutablePointer<CGPoint>
}
typealias CGPathApplierFunction = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<CGPathElement>) -> Void
