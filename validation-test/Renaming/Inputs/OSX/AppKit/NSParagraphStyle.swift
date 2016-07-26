
@available(OSX 10.0, *)
let NSTabColumnTerminatorsAttributeName: String
@available(OSX 10.0, *)
class NSTextTab : NSObject, NSCopying, NSCoding {
  @available(OSX 10.11, *)
  @discardableResult
  class func columnTerminators(for aLocale: NSLocale?) -> NSCharacterSet
  init(textAlignment alignment: NSTextAlignment, location loc: CGFloat, options options: [String : AnyObject] = [:])
  var alignment: NSTextAlignment { get }
  var location: CGFloat { get }
  var options: [String : AnyObject] { get }
}
@available(OSX 10.0, *)
enum NSLineBreakMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case byWordWrapping
  case byCharWrapping
  case byClipping
  case byTruncatingHead
  case byTruncatingTail
  case byTruncatingMiddle
}
@available(OSX 10.0, *)
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
  @available(OSX 10.0, *)
  var tabStops: [NSTextTab] { get }
  @available(OSX 10.0, *)
  var defaultTabInterval: CGFloat { get }
  @available(OSX 10.11, *)
  var allowsDefaultTighteningForTruncation: Bool { get }
  var tighteningFactorForTruncation: Float { get }
  var textBlocks: [NSTextBlock] { get }
  var textLists: [NSTextList] { get }
  var headerLevel: Int { get }
}
@available(OSX 10.0, *)
class NSMutableParagraphStyle : NSParagraphStyle {
  @available(OSX 10.0, *)
  func addTabStop(_ anObject: NSTextTab)
  @available(OSX 10.0, *)
  func removeTabStop(_ anObject: NSTextTab)
  @available(OSX 10.0, *)
  func setParagraphStyle(_ obj: NSParagraphStyle)
}
enum NSTextTabType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case leftTabStopType
  case rightTabStopType
  case centerTabStopType
  case decimalTabStopType
}
extension NSTextTab {
  convenience init(type type: NSTextTabType, location loc: CGFloat)
  var tabStopType: NSTextTabType { get }
}
