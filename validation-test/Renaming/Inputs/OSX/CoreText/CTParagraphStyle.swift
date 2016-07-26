
class CTParagraphStyle {
}
@available(OSX 10.5, *)
@discardableResult
func CTParagraphStyleGetTypeID() -> CFTypeID
enum CTTextAlignment : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  @available(OSX 10.8, *)
  case left
  @available(OSX 10.8, *)
  case right
  @available(OSX 10.8, *)
  case center
  @available(OSX 10.8, *)
  case justified
  @available(OSX 10.8, *)
  case natural
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTLeftTextAlignment: CTTextAlignment { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTRightTextAlignment: CTTextAlignment { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTCenterTextAlignment: CTTextAlignment { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTJustifiedTextAlignment: CTTextAlignment { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
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
@available(OSX 10.5, *)
@discardableResult
func CTParagraphStyleCreate(_ settings: UnsafePointer<CTParagraphStyleSetting>?, _ settingCount: Int) -> CTParagraphStyle
@available(OSX 10.5, *)
@discardableResult
func CTParagraphStyleCreateCopy(_ paragraphStyle: CTParagraphStyle) -> CTParagraphStyle
@available(OSX 10.5, *)
@discardableResult
func CTParagraphStyleGetValueForSpecifier(_ paragraphStyle: CTParagraphStyle, _ spec: CTParagraphStyleSpecifier, _ valueBufferSize: Int, _ valueBuffer: UnsafeMutablePointer<Void>) -> Bool
