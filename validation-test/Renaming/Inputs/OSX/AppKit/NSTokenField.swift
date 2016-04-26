
class NSTokenField : NSTextField {
  var tokenStyle: NSTokenStyle
  var completionDelay: NSTimeInterval
  @discardableResult
  class func defaultCompletionDelay() -> NSTimeInterval
  @NSCopying var tokenizingCharacterSet: NSCharacterSet!
  @discardableResult
  class func defaultTokenizingCharacterSet() -> NSCharacterSet
}
protocol NSTokenFieldDelegate : NSTextFieldDelegate {
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, completionsForSubstring substring: String, indexOfToken tokenIndex: Int, indexOfSelectedItem selectedIndex: UnsafeMutablePointer<Int>?) -> [AnyObject]?
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, shouldAdd tokens: [AnyObject], at index: Int) -> [AnyObject]
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, displayStringForRepresentedObject representedObject: AnyObject) -> String?
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, editingStringForRepresentedObject representedObject: AnyObject) -> String?
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, representedObjectForEditing editingString: String) -> AnyObject
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, writeRepresentedObjects objects: [AnyObject], to pboard: NSPasteboard) -> Bool
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, readFrom pboard: NSPasteboard) -> [AnyObject]?
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, menuForRepresentedObject representedObject: AnyObject) -> NSMenu?
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, hasMenuForRepresentedObject representedObject: AnyObject) -> Bool
  @discardableResult
  optional func tokenField(_ tokenField: NSTokenField, styleForRepresentedObject representedObject: AnyObject) -> NSTokenStyle
}
