
@available(iOS 8.0, *)
enum NSFormattingContext : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case dynamic
  case standalone
  case listItem
  case beginningOfSentence
  case middleOfSentence
}
@available(iOS 8.0, *)
enum NSFormattingUnitStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case short
  case medium
  case long
}
class NSFormatter : NSObject, NSCopying, NSCoding {
  @discardableResult
  func string(for obj: AnyObject) -> String?
  @discardableResult
  func attributedString(for obj: AnyObject, withDefaultAttributes attrs: [String : AnyObject]? = [:]) -> NSAttributedString?
  @discardableResult
  func editingString(for obj: AnyObject) -> String?
  @discardableResult
  func getObjectValue(_ obj: AutoreleasingUnsafeMutablePointer<AnyObject?>?, for string: String, errorDescription error: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
  @discardableResult
  func isPartialStringValid(_ partialString: String, newEditingString newString: AutoreleasingUnsafeMutablePointer<NSString?>?, errorDescription error: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
  @discardableResult
  func isPartialStringValid(_ partialStringPtr: AutoreleasingUnsafeMutablePointer<NSString>, proposedSelectedRange proposedSelRangePtr: NSRangePointer?, originalString origString: String, originalSelectedRange origSelRange: NSRange, errorDescription error: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
}
