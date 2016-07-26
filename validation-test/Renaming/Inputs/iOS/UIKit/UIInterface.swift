
enum UIBarStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case black
  static var blackOpaque: UIBarStyle { get }
  case blackTranslucent
}
@available(iOS 8.0, *)
enum UIUserInterfaceSizeClass : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unspecified
  case compact
  case regular
}
extension UIColor {
  @discardableResult
  class func lightText() -> UIColor
  @discardableResult
  class func darkText() -> UIColor
  @discardableResult
  class func groupTableViewBackground() -> UIColor
}
extension UIFont {
  @discardableResult
  class func labelSize() -> CGFloat
  @discardableResult
  class func buttonFontSize() -> CGFloat
  @discardableResult
  class func smallSystemFontSize() -> CGFloat
  @discardableResult
  class func systemFontSize() -> CGFloat
}
