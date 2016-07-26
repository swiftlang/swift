
class CTParagraphStyle {
}
@available(watchOS 2.0, *)
@discardableResult
func CTParagraphStyleGetTypeID() -> CFTypeID
enum CTTextAlignment : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  @available(watchOS 2.0, *)
  case left
  @available(watchOS 2.0, *)
  case right
  @available(watchOS 2.0, *)
  case center
  @available(watchOS 2.0, *)
  case justified
  @available(watchOS 2.0, *)
  case natural
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTLeftTextAlignment: CTTextAlignment { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTRightTextAlignment: CTTextAlignment { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTCenterTextAlignment: CTTextAlignment { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTJustifiedTextAlignment: CTTextAlignment { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTNaturalTextAlignment: CTTextAlignment { get }
}
enum CTLineBreakMode : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case byWordWrapping
  case byCharWrapping
  case byClipping
  case byTruncatingHead
  case byTruncatingTail
  case byTruncatingMiddle
}
enum CTWritingDirection : Int8 {
  init?(rawValue rawValue: Int8)
  var rawValue: Int8 { get }
  case natural
  case leftToRight
  case rightToLeft
}
enum CTParagraphStyleSpecifier : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case alignment
  case firstLineHeadIndent
  case headIndent
  case tailIndent
  case tabStops
  case defaultTabInterval
  case lineBreakMode
  case lineHeightMultiple
  case maximumLineHeight
  case minimumLineHeight
  case lineSpacing
  case paragraphSpacing
  case paragraphSpacingBefore
  case baseWritingDirection
  case maximumLineSpacing
  case minimumLineSpacing
  case lineSpacingAdjustment
  case lineBoundsOptions
  case count
}
struct CTParagraphStyleSetting {
  var spec: CTParagraphStyleSpecifier
  var valueSize: Int
  var value: UnsafePointer<Void>
}
@available(watchOS 2.0, *)
@discardableResult
func CTParagraphStyleCreate(_ settings: UnsafePointer<CTParagraphStyleSetting>?, _ settingCount: Int) -> CTParagraphStyle
@available(watchOS 2.0, *)
@discardableResult
func CTParagraphStyleCreateCopy(_ paragraphStyle: CTParagraphStyle) -> CTParagraphStyle
@available(watchOS 2.0, *)
@discardableResult
func CTParagraphStyleGetValueForSpecifier(_ paragraphStyle: CTParagraphStyle, _ spec: CTParagraphStyleSpecifier, _ valueBufferSize: Int, _ valueBuffer: UnsafeMutablePointer<Void>) -> Bool
