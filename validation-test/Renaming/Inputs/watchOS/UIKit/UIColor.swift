
@available(watchOS 2.0, *)
class UIColor : NSObject, NSSecureCoding, NSCopying {
  init(white white: CGFloat, alpha alpha: CGFloat)
  init(hue hue: CGFloat, saturation saturation: CGFloat, brightness brightness: CGFloat, alpha alpha: CGFloat)
  init(red red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  init(cgColor cgColor: CGColor)
  init(patternImage image: UIImage)
  @discardableResult
  class func black() -> UIColor
  @discardableResult
  class func darkGray() -> UIColor
  @discardableResult
  class func lightGray() -> UIColor
  @discardableResult
  class func white() -> UIColor
  @discardableResult
  class func gray() -> UIColor
  @discardableResult
  class func red() -> UIColor
  @discardableResult
  class func green() -> UIColor
  @discardableResult
  class func blue() -> UIColor
  @discardableResult
  class func cyan() -> UIColor
  @discardableResult
  class func yellow() -> UIColor
  @discardableResult
  class func magenta() -> UIColor
  @discardableResult
  class func orange() -> UIColor
  @discardableResult
  class func purple() -> UIColor
  @discardableResult
  class func brown() -> UIColor
  @discardableResult
  class func clear() -> UIColor
  func set()
  func setFill()
  func setStroke()
  @available(watchOS 2.0, *)
  @discardableResult
  func getWhite(_ white: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func getHue(_ hue: UnsafeMutablePointer<CGFloat>?, saturation saturation: UnsafeMutablePointer<CGFloat>?, brightness brightness: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func getRed(_ red: UnsafeMutablePointer<CGFloat>?, green green: UnsafeMutablePointer<CGFloat>?, blue blue: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?) -> Bool
  @discardableResult
  func withAlphaComponent(_ alpha: CGFloat) -> UIColor
  var cgColor: CGColor { get }
}

extension UIColor : _ColorLiteralConvertible {
}
