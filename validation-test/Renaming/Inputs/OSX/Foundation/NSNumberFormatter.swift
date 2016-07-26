
enum NSNumberFormatterBehavior : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case behaviorDefault
  case behavior10_0
  case behavior10_4
}
class NSNumberFormatter : NSFormatter {
  @available(OSX 10.10, *)
  var formattingContext: NSFormattingContext
  func getObjectValue(_ obj: AutoreleasingUnsafeMutablePointer<AnyObject?>?, for string: String, range rangep: UnsafeMutablePointer<NSRange>?) throws
  @discardableResult
  func string(from number: NSNumber) -> String?
  @discardableResult
  func number(from string: String) -> NSNumber?
  @available(OSX 10.6, *)
  @discardableResult
  class func localizedString(from num: NSNumber, number nstyle: NSNumberFormatterStyle) -> String
  @discardableResult
  class func defaultFormatterBehavior() -> NSNumberFormatterBehavior
  class func setDefaultFormatterBehavior(_ behavior: NSNumberFormatterBehavior)
  var numberStyle: NSNumberFormatterStyle
  @NSCopying var locale: NSLocale!
  var generatesDecimalNumbers: Bool
  var formatterBehavior: NSNumberFormatterBehavior
  var negativeFormat: String!
  var textAttributesForNegativeValues: [String : AnyObject]?
  var positiveFormat: String!
  var textAttributesForPositiveValues: [String : AnyObject]?
  var allowsFloats: Bool
  var decimalSeparator: String!
  var alwaysShowsDecimalSeparator: Bool
  var currencyDecimalSeparator: String!
  var usesGroupingSeparator: Bool
  var groupingSeparator: String!
  var zeroSymbol: String?
  var textAttributesForZero: [String : AnyObject]?
  var nilSymbol: String
  var textAttributesForNil: [String : AnyObject]?
  var notANumberSymbol: String!
  var textAttributesForNotANumber: [String : AnyObject]?
  var positiveInfinitySymbol: String
  var textAttributesForPositiveInfinity: [String : AnyObject]?
  var negativeInfinitySymbol: String
  var textAttributesForNegativeInfinity: [String : AnyObject]?
  var positivePrefix: String!
  var positiveSuffix: String!
  var negativePrefix: String!
  var negativeSuffix: String!
  var currencyCode: String!
  var currencySymbol: String!
  var internationalCurrencySymbol: String!
  var percentSymbol: String!
  var perMillSymbol: String!
  var minusSign: String!
  var plusSign: String!
  var exponentSymbol: String!
  var groupingSize: Int
  var secondaryGroupingSize: Int
  @NSCopying var multiplier: NSNumber?
  var formatWidth: Int
  var paddingCharacter: String!
  var paddingPosition: NSNumberFormatterPadPosition
  var roundingMode: NSNumberFormatterRoundingMode
  @NSCopying var roundingIncrement: NSNumber!
  var minimumIntegerDigits: Int
  var maximumIntegerDigits: Int
  var minimumFractionDigits: Int
  var maximumFractionDigits: Int
  @NSCopying var minimum: NSNumber?
  @NSCopying var maximum: NSNumber?
  @available(OSX 10.5, *)
  var currencyGroupingSeparator: String!
  @available(OSX 10.5, *)
  var isLenient: Bool
  @available(OSX 10.5, *)
  var usesSignificantDigits: Bool
  @available(OSX 10.5, *)
  var minimumSignificantDigits: Int
  @available(OSX 10.5, *)
  var maximumSignificantDigits: Int
  @available(OSX 10.5, *)
  var isPartialStringValidationEnabled: Bool
}
enum NSNumberFormatterStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noStyle
  case decimalStyle
  case currencyStyle
  case percentStyle
  case scientificStyle
  case spellOutStyle
  @available(OSX 10.11, *)
  case ordinalStyle
  @available(OSX 10.11, *)
  case currencyISOCodeStyle
  @available(OSX 10.11, *)
  case currencyPluralStyle
  @available(OSX 10.11, *)
  case currencyAccountingStyle
}
enum NSNumberFormatterPadPosition : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case beforePrefix
  case afterPrefix
  case beforeSuffix
  case afterSuffix
}
enum NSNumberFormatterRoundingMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case roundCeiling
  case roundFloor
  case roundDown
  case roundUp
  case roundHalfEven
  case roundHalfDown
  case roundHalfUp
}
extension NSNumberFormatter {
  var hasThousandSeparators: Bool
  var thousandSeparator: String!
  var localizesFormat: Bool
  var format: String
  @NSCopying var attributedStringForZero: NSAttributedString
  @NSCopying var attributedStringForNil: NSAttributedString
  @NSCopying var attributedStringForNotANumber: NSAttributedString
  @NSCopying var roundingBehavior: NSDecimalNumberHandler
}
