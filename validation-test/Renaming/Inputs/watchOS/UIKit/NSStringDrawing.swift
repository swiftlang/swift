
@available(watchOS 2.0, *)
class NSStringDrawingContext : NSObject {
  var minimumScaleFactor: CGFloat
  var actualScaleFactor: CGFloat { get }
  var totalBounds: CGRect { get }
}
extension NSString {
  @available(watchOS 2.0, *)
  @discardableResult
  func size(attributes attrs: [String : AnyObject]? = [:]) -> CGSize
  @available(watchOS 2.0, *)
  func draw(at point: CGPoint, withAttributes attrs: [String : AnyObject]? = [:])
  @available(watchOS 2.0, *)
  func draw(in rect: CGRect, withAttributes attrs: [String : AnyObject]? = [:])
}
extension NSAttributedString {
  @available(watchOS 2.0, *)
  @discardableResult
  func size() -> CGSize
  @available(watchOS 2.0, *)
  func draw(at point: CGPoint)
  @available(watchOS 2.0, *)
  func draw(in rect: CGRect)
}
@available(watchOS 2.0, *)
struct NSStringDrawingOptions : OptionSet {
  init(rawValue rawValue: Int)
  let rawValue: Int
  static var usesLineFragmentOrigin: NSStringDrawingOptions { get }
  static var usesFontLeading: NSStringDrawingOptions { get }
  static var usesDeviceMetrics: NSStringDrawingOptions { get }
  @available(watchOS 2.0, *)
  static var truncatesLastVisibleLine: NSStringDrawingOptions { get }
}
extension NSString {
  @available(watchOS 2.0, *)
  func draw(with rect: CGRect, options options: NSStringDrawingOptions = [], attributes attributes: [String : AnyObject]? = [:], context context: NSStringDrawingContext?)
  @available(watchOS 2.0, *)
  @discardableResult
  func boundingRect(with size: CGSize, options options: NSStringDrawingOptions = [], attributes attributes: [String : AnyObject]? = [:], context context: NSStringDrawingContext?) -> CGRect
}
extension NSAttributedString {
  @available(watchOS 2.0, *)
  func draw(with rect: CGRect, options options: NSStringDrawingOptions = [], context context: NSStringDrawingContext?)
  @available(watchOS 2.0, *)
  @discardableResult
  func boundingRect(with size: CGSize, options options: NSStringDrawingOptions = [], context context: NSStringDrawingContext?) -> CGRect
}
extension NSStringDrawingContext {
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  var minimumTrackingAdjustment: CGFloat
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  var actualTrackingAdjustment: CGFloat { get }
}
