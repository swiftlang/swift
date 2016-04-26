
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
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.2, *)
  init?(copy path: CGPath?)
  @available(OSX 10.7, *)
  init?(copyByTransforming path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(OSX 10.5, *)
  init(with rect: CGRect, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(OSX 10.7, *)
  init(withEllipseIn rect: CGRect, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(OSX 10.9, *)
  init(withRoundedRect rect: CGRect, cornerWidth cornerWidth: CGFloat, cornerHeight cornerHeight: CGFloat, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(OSX 10.7, *)
  init?(copyByDashingPath path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?, phase phase: CGFloat, lengths lengths: UnsafePointer<CGFloat>?, count count: Int)
  @available(OSX 10.7, *)
  init?(copyByStroking path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?, lineWidth lineWidth: CGFloat, lineCap lineCap: CGLineCap, lineJoin lineJoin: CGLineJoin, miterLimit miterLimit: CGFloat)
  @available(OSX 10.2, *)
  @discardableResult
  func equalTo(_ path2: CGPath?) -> Bool
  @available(OSX 10.2, *)
  @discardableResult
  func isEmpty() -> Bool
  @available(OSX 10.2, *)
  @discardableResult
  func isRect(_ rect: UnsafeMutablePointer<CGRect>?) -> Bool
  @available(OSX 10.2, *)
  var currentPoint: CGPoint { get }
  @available(OSX 10.2, *)
  var boundingBox: CGRect { get }
  @available(OSX 10.6, *)
  var boundingBoxOfPath: CGRect { get }
  @available(OSX 10.4, *)
  @discardableResult
  func containsPoint(_ m: UnsafePointer<CGAffineTransform>?, point point: CGPoint, eoFill eoFill: Bool) -> Bool
  @available(OSX 10.2, *)
  func apply(info info: UnsafeMutablePointer<Void>?, function function: CGPathApplierFunction?)
}
extension CGMutablePath {
  @available(OSX 10.2, *)
  init()
  @available(OSX 10.2, *)
  init?(copy path: CGPath?)
  @available(OSX 10.7, *)
  init?(copyByTransforming path: CGPath?, transform transform: UnsafePointer<CGAffineTransform>?)
  @available(OSX 10.9, *)
  func addRoundedRect(_ transform: UnsafePointer<CGAffineTransform>?, rect rect: CGRect, cornerWidth cornerWidth: CGFloat, cornerHeight cornerHeight: CGFloat)
  @available(OSX 10.2, *)
  func moveTo(_ m: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat)
  @available(OSX 10.2, *)
  func addLineTo(_ m: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat)
  @available(OSX 10.2, *)
  func addQuadCurve(_ m: UnsafePointer<CGAffineTransform>?, cpx cpx: CGFloat, cpy cpy: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(OSX 10.2, *)
  func addCurve(_ m: UnsafePointer<CGAffineTransform>?, cp1x cp1x: CGFloat, cp1y cp1y: CGFloat, cp2x cp2x: CGFloat, cp2y cp2y: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(OSX 10.2, *)
  func closeSubpath()
  @available(OSX 10.2, *)
  func addRect(_ m: UnsafePointer<CGAffineTransform>?, rect rect: CGRect)
  @available(OSX 10.2, *)
  func addRects(_ m: UnsafePointer<CGAffineTransform>?, rects rects: UnsafePointer<CGRect>?, count count: Int)
  @available(OSX 10.2, *)
  func addLines(_ m: UnsafePointer<CGAffineTransform>?, between points: UnsafePointer<CGPoint>?, count count: Int)
  @available(OSX 10.4, *)
  func addEllipseIn(_ m: UnsafePointer<CGAffineTransform>?, rect rect: CGRect)
  @available(OSX 10.7, *)
  func addRelativeArc(matrix matrix: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat, radius radius: CGFloat, startAngle startAngle: CGFloat, delta delta: CGFloat)
  @available(OSX 10.2, *)
  func addArc(_ m: UnsafePointer<CGAffineTransform>?, x x: CGFloat, y y: CGFloat, radius radius: CGFloat, startAngle startAngle: CGFloat, endAngle endAngle: CGFloat, clockwise clockwise: Bool)
  @available(OSX 10.2, *)
  func addArc(_ m: UnsafePointer<CGAffineTransform>?, x1 x1: CGFloat, y1 y1: CGFloat, x2 x2: CGFloat, y2 y2: CGFloat, radius radius: CGFloat)
  @available(OSX 10.2, *)
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
