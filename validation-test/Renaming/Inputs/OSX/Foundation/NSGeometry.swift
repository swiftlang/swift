
typealias NSPoint = CGPoint
typealias NSPointPointer = UnsafeMutablePointer<NSPoint>
typealias NSPointArray = UnsafeMutablePointer<NSPoint>
typealias NSSize = CGSize
typealias NSSizePointer = UnsafeMutablePointer<NSSize>
typealias NSSizeArray = UnsafeMutablePointer<NSSize>
typealias NSRect = CGRect
typealias NSRectPointer = UnsafeMutablePointer<NSRect>
typealias NSRectArray = UnsafeMutablePointer<NSRect>
enum NSRectEdge : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case minX
  case minY
  case maxX
  case maxY
}

extension NSRectEdge {
  init(rectEdge rectEdge: CGRectEdge)
}
struct NSEdgeInsets {
  var top: CGFloat
  var left: CGFloat
  var bottom: CGFloat
  var right: CGFloat
  init()
  init(top top: CGFloat, left left: CGFloat, bottom bottom: CGFloat, right right: CGFloat)
}
struct NSAlignmentOptions : OptionSet {
  init(rawValue rawValue: UInt64)
  let rawValue: UInt64
  static var alignMinXInward: NSAlignmentOptions { get }
  static var alignMinYInward: NSAlignmentOptions { get }
  static var alignMaxXInward: NSAlignmentOptions { get }
  static var alignMaxYInward: NSAlignmentOptions { get }
  static var alignWidthInward: NSAlignmentOptions { get }
  static var alignHeightInward: NSAlignmentOptions { get }
  static var alignMinXOutward: NSAlignmentOptions { get }
  static var alignMinYOutward: NSAlignmentOptions { get }
  static var alignMaxXOutward: NSAlignmentOptions { get }
  static var alignMaxYOutward: NSAlignmentOptions { get }
  static var alignWidthOutward: NSAlignmentOptions { get }
  static var alignHeightOutward: NSAlignmentOptions { get }
  static var alignMinXNearest: NSAlignmentOptions { get }
  static var alignMinYNearest: NSAlignmentOptions { get }
  static var alignMaxXNearest: NSAlignmentOptions { get }
  static var alignMaxYNearest: NSAlignmentOptions { get }
  static var alignWidthNearest: NSAlignmentOptions { get }
  static var alignHeightNearest: NSAlignmentOptions { get }
  static var alignRectFlipped: NSAlignmentOptions { get }
  static var alignAllEdgesInward: NSAlignmentOptions { get }
  static var alignAllEdgesOutward: NSAlignmentOptions { get }
  static var alignAllEdgesNearest: NSAlignmentOptions { get }
}
let NSZeroPoint: NSPoint
let NSZeroSize: NSSize
let NSZeroRect: NSRect
@available(OSX 10.10, *)
let NSEdgeInsetsZero: NSEdgeInsets
@discardableResult
func NSMakePoint(_ x: CGFloat, _ y: CGFloat) -> NSPoint
@discardableResult
func NSMakeSize(_ w: CGFloat, _ h: CGFloat) -> NSSize
@discardableResult
func NSMakeRect(_ x: CGFloat, _ y: CGFloat, _ w: CGFloat, _ h: CGFloat) -> NSRect
@discardableResult
func NSMaxX(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSMaxY(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSMidX(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSMidY(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSMinX(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSMinY(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSWidth(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSHeight(_ aRect: NSRect) -> CGFloat
@discardableResult
func NSRectFromCGRect(_ cgrect: CGRect) -> NSRect
@discardableResult
func NSRectToCGRect(_ nsrect: NSRect) -> CGRect
@discardableResult
func NSPointFromCGPoint(_ cgpoint: CGPoint) -> NSPoint
@discardableResult
func NSPointToCGPoint(_ nspoint: NSPoint) -> CGPoint
@discardableResult
func NSSizeFromCGSize(_ cgsize: CGSize) -> NSSize
@discardableResult
func NSSizeToCGSize(_ nssize: NSSize) -> CGSize
@discardableResult
func NSEdgeInsetsMake(_ top: CGFloat, _ left: CGFloat, _ bottom: CGFloat, _ right: CGFloat) -> NSEdgeInsets
@discardableResult
func NSEqualPoints(_ aPoint: NSPoint, _ bPoint: NSPoint) -> Bool
@discardableResult
func NSEqualSizes(_ aSize: NSSize, _ bSize: NSSize) -> Bool
@discardableResult
func NSEqualRects(_ aRect: NSRect, _ bRect: NSRect) -> Bool
@discardableResult
func NSIsEmptyRect(_ aRect: NSRect) -> Bool
@available(OSX 10.10, *)
@discardableResult
func NSEdgeInsetsEqual(_ aInsets: NSEdgeInsets, _ bInsets: NSEdgeInsets) -> Bool
@discardableResult
func NSInsetRect(_ aRect: NSRect, _ dX: CGFloat, _ dY: CGFloat) -> NSRect
@discardableResult
func NSIntegralRect(_ aRect: NSRect) -> NSRect
@available(OSX 10.7, *)
@discardableResult
func NSIntegralRectWithOptions(_ aRect: NSRect, _ opts: NSAlignmentOptions) -> NSRect
@discardableResult
func NSUnionRect(_ aRect: NSRect, _ bRect: NSRect) -> NSRect
@discardableResult
func NSIntersectionRect(_ aRect: NSRect, _ bRect: NSRect) -> NSRect
@discardableResult
func NSOffsetRect(_ aRect: NSRect, _ dX: CGFloat, _ dY: CGFloat) -> NSRect
func NSDivideRect(_ inRect: NSRect, _ slice: UnsafeMutablePointer<NSRect>, _ rem: UnsafeMutablePointer<NSRect>, _ amount: CGFloat, _ edge: NSRectEdge)
@discardableResult
func NSPointInRect(_ aPoint: NSPoint, _ aRect: NSRect) -> Bool
@discardableResult
func NSMouseInRect(_ aPoint: NSPoint, _ aRect: NSRect, _ flipped: Bool) -> Bool
@discardableResult
func NSContainsRect(_ aRect: NSRect, _ bRect: NSRect) -> Bool
@discardableResult
func NSIntersectsRect(_ aRect: NSRect, _ bRect: NSRect) -> Bool
@discardableResult
func NSStringFromPoint(_ aPoint: NSPoint) -> String
@discardableResult
func NSStringFromSize(_ aSize: NSSize) -> String
@discardableResult
func NSStringFromRect(_ aRect: NSRect) -> String
@discardableResult
func NSPointFromString(_ aString: String) -> NSPoint
@discardableResult
func NSSizeFromString(_ aString: String) -> NSSize
@discardableResult
func NSRectFromString(_ aString: String) -> NSRect
extension NSValue {
  /*not inherited*/ init(point point: NSPoint)
  /*not inherited*/ init(size size: NSSize)
  /*not inherited*/ init(rect rect: NSRect)
  @available(OSX 10.10, *)
  /*not inherited*/ init(edgeInsets insets: NSEdgeInsets)
  var pointValue: NSPoint { get }
  var sizeValue: NSSize { get }
  var rectValue: NSRect { get }
  @available(OSX 10.10, *)
  var edgeInsetsValue: NSEdgeInsets { get }
}
extension NSCoder {
  func encode(_ point: NSPoint)
  @discardableResult
  func decodePoint() -> NSPoint
  func encode(_ size: NSSize)
  @discardableResult
  func decodeSize() -> NSSize
  func encode(_ rect: NSRect)
  @discardableResult
  func decodeRect() -> NSRect
}
extension NSCoder {
  func encode(_ point: NSPoint, forKey key: String)
  func encode(_ size: NSSize, forKey key: String)
  func encode(_ rect: NSRect, forKey key: String)
  @discardableResult
  func decodePoint(forKey key: String) -> NSPoint
  @discardableResult
  func decodeSize(forKey key: String) -> NSSize
  @discardableResult
  func decodeRect(forKey key: String) -> NSRect
}
