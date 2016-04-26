
@available(tvOS 6.0, *)
class NSStringDrawingContext : NSObject {
  var minimumScaleFactor: CGFloat
  var actualScaleFactor: CGFloat { get }
  var totalBounds: CGRect { get }
}
extension NSString {
  @available(tvOS 7.0, *)
  @discardableResult
  func size(attributes attrs: [String : AnyObject]? = [:]) -> CGSize
  @available(tvOS 7.0, *)
  func draw(at point: CGPoint, withAttributes attrs: [String : AnyObject]? = [:])
  @available(tvOS 7.0, *)
  func draw(in rect: CGRect, withAttributes attrs: [String : AnyObject]? = [:])
}
extension NSAttributedString {
  @available(tvOS 6.0, *)
  @discardableResult
  func size() -> CGSize
  @available(tvOS 6.0, *)
  func draw(at point: CGPoint)
  @available(tvOS 6.0, *)
  func draw(in rect: CGRect)
}
@available(tvOS 6.0, *)
struct NSStringDrawingOptions : OptionSet {
  init(rawValue rawValue: Int)
  let rawValue: Int
  static var usesLineFragmentOrigin: NSStringDrawingOptions { get }
  static var usesFontLeading: NSStringDrawingOptions { get }
  static var usesDeviceMetrics: NSStringDrawingOptions { get }
  @available(tvOS 6.0, *)
  static var truncatesLastVisibleLine: NSStringDrawingOptions { get }
}
extension NSString {
  @available(tvOS 7.0, *)
  func draw(with rect: CGRect, options options: NSStringDrawingOptions = [], attributes attributes: [String : AnyObject]? = [:], context context: NSStringDrawingContext?)
  @available(tvOS 7.0, *)
  @discardableResult
  func boundingRect(with size: CGSize, options options: NSStringDrawingOptions = [], attributes attributes: [String : AnyObject]? = [:], context context: NSStringDrawingContext?) -> CGRect
}
extension NSAttributedString {
  @available(tvOS 6.0, *)
  func draw(with rect: CGRect, options options: NSStringDrawingOptions = [], context context: NSStringDrawingContext?)
  @available(tvOS 6.0, *)
  @discardableResult
  func boundingRect(with size: CGSize, options options: NSStringDrawingOptions = [], context context: NSStringDrawingContext?) -> CGRect
}
extension NSStringDrawingContext {
}
