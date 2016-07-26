
class NSTextField : NSControl, NSUserInterfaceValidations, NSAccessibilityNavigableStaticText {
  @available(OSX 10.10, *)
  var placeholderString: String?
  @available(OSX 10.10, *)
  @NSCopying var placeholderAttributedString: NSAttributedString?
  @NSCopying var backgroundColor: NSColor?
  var drawsBackground: Bool
  @NSCopying var textColor: NSColor?
  var isBordered: Bool
  var isBezeled: Bool
  var isEditable: Bool
  var isSelectable: Bool
  func selectText(_ sender: AnyObject?)
  unowned(unsafe) var delegate: @sil_unmanaged NSTextFieldDelegate?
  @discardableResult
  func textShouldBeginEditing(_ textObject: NSText) -> Bool
  @discardableResult
  func textShouldEndEditing(_ textObject: NSText) -> Bool
  func textDidBeginEditing(_ notification: NSNotification)
  func textDidEndEditing(_ notification: NSNotification)
  func textDidChange(_ notification: NSNotification)
  var bezelStyle: NSTextFieldBezelStyle
  @available(OSX 10.8, *)
  var preferredMaxLayoutWidth: CGFloat
  @available(OSX 10.11, *)
  var maximumNumberOfLines: Int
  @available(OSX 10.11, *)
  var allowsDefaultTighteningForTruncation: Bool
}
extension NSTextField {
  var allowsEditingTextAttributes: Bool
  var importsGraphics: Bool
}
protocol NSTextFieldDelegate : NSControlTextEditingDelegate {
}
extension NSTextField {
}
