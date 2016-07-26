
@available(OSX 10.11, *)
enum NSPersonNameComponentsFormatterStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case short
  case medium
  case long
  case abbreviated
}
@available(OSX 10.11, *)
struct NSPersonNameComponentsFormatterOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var phonetic: NSPersonNameComponentsFormatterOptions { get }
}
@available(OSX 10.11, *)
class NSPersonNameComponentsFormatter : NSFormatter {
  var style: NSPersonNameComponentsFormatterStyle
  var isPhonetic: Bool
  @discardableResult
  class func localizedString(from components: NSPersonNameComponents, style nameFormatStyle: NSPersonNameComponentsFormatterStyle, options nameOptions: NSPersonNameComponentsFormatterOptions = []) -> String
  @discardableResult
  func string(from components: NSPersonNameComponents) -> String
  @discardableResult
  func annotatedString(from components: NSPersonNameComponents) -> NSAttributedString
}
@available(OSX 10.11, *)
let NSPersonNameComponentKey: String
@available(OSX 10.11, *)
let NSPersonNameComponentGivenName: String
@available(OSX 10.11, *)
let NSPersonNameComponentFamilyName: String
@available(OSX 10.11, *)
let NSPersonNameComponentMiddleName: String
@available(OSX 10.11, *)
let NSPersonNameComponentPrefix: String
@available(OSX 10.11, *)
let NSPersonNameComponentSuffix: String
@available(OSX 10.11, *)
let NSPersonNameComponentNickname: String
@available(OSX 10.11, *)
let NSPersonNameComponentDelimiter: String
