
enum NSLineCapStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case buttLineCapStyle
  case roundLineCapStyle
  case squareLineCapStyle
}
enum NSLineJoinStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case miterLineJoinStyle
  case roundLineJoinStyle
  case bevelLineJoinStyle
}
enum NSWindingRule : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case nonZeroWindingRule
  case evenOddWindingRule
}
enum NSBezierPathElement : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case moveToBezierPathElement
  case lineToBezierPathElement
  case curveToBezierPathElement
  case closePathBezierPathElement
}
class NSBezierPath : NSObject, NSCopying, NSCoding {
  /*not inherited*/ init(rect rect: NSRect)
  /*not inherited*/ init(ovalIn rect: NSRect)
  @available(OSX 10.5, *)
  /*not inherited*/ init(roundedRect rect: NSRect, xRadius xRadius: CGFloat, yRadius yRadius: CGFloat)
  class func fill(_ rect: NSRect)
  class func stroke(_ rect: NSRect)
  class func clipRect(_ rect: NSRect)
  class func strokeLine(from point1: NSPoint, to point2: NSPoint)
  class func drawPackedGlyphs(_ packedGlyphs: UnsafePointer<Int8>, at point: NSPoint)
  class func setDefaultMiterLimit(_ limit: CGFloat)
  @discardableResult
  class func defaultMiterLimit() -> CGFloat
  class func setDefaultFlatness(_ flatness: CGFloat)
  @discardableResult
  class func defaultFlatness() -> CGFloat
  class func setDefaultWindingRule(_ windingRule: NSWindingRule)
  @discardableResult
  class func defaultWindingRule() -> NSWindingRule
  class func setDefaultLineCapStyle(_ lineCapStyle: NSLineCapStyle)
  @discardableResult
  class func defaultLineCapStyle() -> NSLineCapStyle
  class func setDefaultLineJoinStyle(_ lineJoinStyle: NSLineJoinStyle)
  @discardableResult
  class func defaultLineJoinStyle() -> NSLineJoinStyle
  class func setDefaultLineWidth(_ lineWidth: CGFloat)
  @discardableResult
  class func defaultLineWidth() -> CGFloat
  func move(to point: NSPoint)
  func line(to point: NSPoint)
  func curve(to endPoint: NSPoint, controlPoint1 controlPoint1: NSPoint, controlPoint2 controlPoint2: NSPoint)
  func close()
  func removeAllPoints()
  func relativeMove(to point: NSPoint)
  func relativeLine(to point: NSPoint)
  func relativeCurve(to endPoint: NSPoint, controlPoint1 controlPoint1: NSPoint, controlPoint2 controlPoint2: NSPoint)
  var lineWidth: CGFloat
  var lineCapStyle: NSLineCapStyle
  var lineJoinStyle: NSLineJoinStyle
  var windingRule: NSWindingRule
  var miterLimit: CGFloat
  var flatness: CGFloat
  func getLineDash(_ pattern: UnsafeMutablePointer<CGFloat>?, count count: UnsafeMutablePointer<Int>?, phase phase: UnsafeMutablePointer<CGFloat>?)
  func setLineDash(_ pattern: UnsafePointer<CGFloat>?, count count: Int, phase phase: CGFloat)
  func stroke()
  func fill()
  func addClip()
  func setClip()
  @NSCopying var flattening: NSBezierPath { get }
  @NSCopying var reversing: NSBezierPath { get }
  func transform(using transform: NSAffineTransform)
  var isEmpty: Bool { get }
  var currentPoint: NSPoint { get }
  var controlPointBounds: NSRect { get }
  var bounds: NSRect { get }
  var elementCount: Int { get }
  @discardableResult
  func element(at index: Int, associatedPoints points: NSPointArray?) -> NSBezierPathElement
  @discardableResult
  func element(at index: Int) -> NSBezierPathElement
  func setAssociatedPoints(_ points: NSPointArray?, at index: Int)
  func append(_ path: NSBezierPath)
  func append(with rect: NSRect)
  func append(with points: NSPointArray, count count: Int)
  func appendWithOval(in rect: NSRect)
  func appendWithArc(withCenter center: NSPoint, radius radius: CGFloat, startAngle startAngle: CGFloat, endAngle endAngle: CGFloat, clockwise clockwise: Bool)
  func appendWithArc(withCenter center: NSPoint, radius radius: CGFloat, startAngle startAngle: CGFloat, endAngle endAngle: CGFloat)
  func appendWithArc(from point1: NSPoint, to point2: NSPoint, radius radius: CGFloat)
  func append(withGlyph glyph: NSGlyph, in font: NSFont)
  func append(withGlyphs glyphs: UnsafeMutablePointer<NSGlyph>, count count: Int, in font: NSFont)
  func append(withPackedGlyphs packedGlyphs: UnsafePointer<Int8>)
  @available(OSX 10.5, *)
  func append(withRoundedRect rect: NSRect, xRadius xRadius: CGFloat, yRadius yRadius: CGFloat)
  @discardableResult
  func contains(_ point: NSPoint) -> Bool
}
extension NSBezierPath {
}
