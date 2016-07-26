
@available(watchOS 2.0, *)
let NSTabColumnTerminatorsAttributeName: String
@available(watchOS 2.0, *)
class NSTextTab : NSObject, NSCopying, NSCoding {
  @available(watchOS 2.0, *)
  @discardableResult
  class func columnTerminators(for aLocale: NSLocale?) -> NSCharacterSet
  init(textAlignment alignment: NSTextAlignment, location loc: CGFloat, options options: [String : AnyObject] = [:])
  var alignment: NSTextAlignment { get }
  var location: CGFloat { get }
  var options: [String : AnyObject] { get }
}
@available(watchOS 2.0, *)
enum NSLineBreakMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case byWordWrapping
  case byCharWrapping
  case byClipping
  case byTruncatingHead
  case byTruncatingTail
  case byTruncatingMiddle
}
@available(watchOS 2.0, *)
class NSParagraphStyle : NSObject, NSCopying, NSMutableCopying, NSSecureCoding {
  @discardableResult
  class func defaultParagraphStyle() -> NSParagraphStyle
  @discardableResult
  class func defaultWritingDirection(forLanguage languageName: String?) -> NSWritingDirection
  var lineSpacing: CGFloat { get }
  var paragraphSpacing: CGFloat { get }
  var alignment: NSTextAlignment { get }
  var headIndent: CGFloat { get }
  var tailIndent: CGFloat { get }
  var firstLineHeadIndent: CGFloat { get }
  var minimumLineHeight: CGFloat { get }
  var maximumLineHeight: CGFloat { get }
  var lineBreakMode: NSLineBreakMode { get }
  var baseWritingDirection: NSWritingDirection { get }
  var lineHeightMultiple: CGFloat { get }
  var paragraphSpacingBefore: CGFloat { get }
  var hyphenationFactor: Float { get }
  @available(watchOS 2.0, *)
  var tabStops: [NSTextTab] { get }
  @available(watchOS 2.0, *)
  var defaultTabInterval: CGFloat { get }
  @available(watchOS 2.0, *)
  var allowsDefaultTighteningForTruncation: Bool { get }
}
@available(watchOS 2.0, *)
class NSMutableParagraphStyle : NSParagraphStyle {
  @available(watchOS 2.0, *)
  func addTabStop(_ anObject: NSTextTab)
  @available(watchOS 2.0, *)
  func removeTabStop(_ anObject: NSTextTab)
  @available(watchOS 2.0, *)
  func setParagraphStyle(_ obj: NSParagraphStyle)
}
