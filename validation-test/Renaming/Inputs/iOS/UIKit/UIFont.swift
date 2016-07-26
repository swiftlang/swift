
@available(iOS 2.0, *)
class UIFont : NSObject, NSCopying {
  @available(iOS 7.0, *)
  @discardableResult
  class func preferredFont(forTextStyle style: String) -> UIFont
  /*not inherited*/ init?(name fontName: String, size fontSize: CGFloat)
  @discardableResult
  class func familyNames() -> [String]
  @discardableResult
  class func fontNames(forFamilyName familyName: String) -> [String]
  @discardableResult
  class func systemFont(ofSize fontSize: CGFloat) -> UIFont
  @discardableResult
  class func boldSystemFont(ofSize fontSize: CGFloat) -> UIFont
  @discardableResult
  class func italicSystemFont(ofSize fontSize: CGFloat) -> UIFont
  @available(iOS 8.2, *)
  @discardableResult
  class func systemFont(ofSize fontSize: CGFloat, weight weight: CGFloat) -> UIFont
  @available(iOS 9.0, *)
  @discardableResult
  class func monospacedDigitSystemFont(ofSize fontSize: CGFloat, weight weight: CGFloat) -> UIFont
  var familyName: String { get }
  var fontName: String { get }
  var pointSize: CGFloat { get }
  var ascender: CGFloat { get }
  var descender: CGFloat { get }
  var capHeight: CGFloat { get }
  var xHeight: CGFloat { get }
  @available(iOS 4.0, *)
  var lineHeight: CGFloat { get }
  var leading: CGFloat { get }
  @discardableResult
  func withSize(_ fontSize: CGFloat) -> UIFont
  @available(iOS 7.0, *)
  /*not inherited*/ init(descriptor descriptor: UIFontDescriptor, size pointSize: CGFloat)
  @available(iOS 7.0, *)
  @discardableResult
  func fontDescriptor() -> UIFontDescriptor
}
