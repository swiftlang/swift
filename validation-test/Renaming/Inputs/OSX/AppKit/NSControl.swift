
class NSControl : NSView {
  weak var target: @sil_weak AnyObject?
  var action: Selector?
  var ignoresMultiClick: Bool
  var isContinuous: Bool
  var isEnabled: Bool
  var refusesFirstResponder: Bool
  @available(OSX 10.10, *)
  var isHighlighted: Bool
  @available(OSX 10.10, *)
  var controlSize: NSControlSize
  var formatter: NSFormatter?
  var stringValue: String
  @NSCopying var attributedStringValue: NSAttributedString
  @NSCopying var objectValue: AnyObject?
  var intValue: Int32
  var integerValue: Int
  var floatValue: Float
  var doubleValue: Double
  @available(OSX 10.10, *)
  @discardableResult
  func sizeThatFits(_ size: NSSize) -> NSSize
  func sizeToFit()
  @discardableResult
  func sendAction(on mask: Int) -> Int
  @discardableResult
  func sendAction(_ theAction: Selector, to theTarget: AnyObject?) -> Bool
  func takeIntValueFrom(_ sender: AnyObject?)
  func takeFloatValueFrom(_ sender: AnyObject?)
  func takeDoubleValueFrom(_ sender: AnyObject?)
  func takeStringValueFrom(_ sender: AnyObject?)
  func takeObjectValueFrom(_ sender: AnyObject?)
  @available(OSX 10.5, *)
  func takeIntegerValueFrom(_ sender: AnyObject?)
}
struct __conFlags {
  var enabled: UInt32
  var ignoreMultiClick: UInt32
  var calcSize: UInt32
  var drawingAncestor: UInt32
  var ibReserved: UInt32
  var updateCellFocus: UInt32
  var allowsLogicalLayoutDirection: UInt32
  var asmlwidth: UInt32
  var hsmlwidth: UInt32
  var dontValidate: UInt32
  var reserved: UInt32
  init()
  init(enabled enabled: UInt32, ignoreMultiClick ignoreMultiClick: UInt32, calcSize calcSize: UInt32, drawingAncestor drawingAncestor: UInt32, ibReserved ibReserved: UInt32, updateCellFocus updateCellFocus: UInt32, allowsLogicalLayoutDirection allowsLogicalLayoutDirection: UInt32, asmlwidth asmlwidth: UInt32, hsmlwidth hsmlwidth: UInt32, dontValidate dontValidate: UInt32, reserved reserved: UInt32)
}
extension NSControl {
  func performClick(_ sender: AnyObject?)
}
extension NSControl {
  var alignment: NSTextAlignment
  @NSCopying var font: NSFont?
  @available(OSX 10.10, *)
  var lineBreakMode: NSLineBreakMode
  @available(OSX 10.10, *)
  var usesSingleLineMode: Bool
  var baseWritingDirection: NSWritingDirection
  @available(OSX 10.8, *)
  var allowsExpansionToolTips: Bool
  @available(OSX 10.10, *)
  @discardableResult
  func expansionFrame(withFrame contentFrame: NSRect) -> NSRect
  @available(OSX 10.10, *)
  func draw(withExpansionFrame contentFrame: NSRect, in view: NSView)
}
extension NSControl {
  @discardableResult
  func currentEditor() -> NSText?
  @discardableResult
  func abortEditing() -> Bool
  func validateEditing()
  @available(OSX 10.10, *)
  func edit(withFrame aRect: NSRect, editor textObj: NSText, delegate anObject: AnyObject?, event theEvent: NSEvent)
  @available(OSX 10.10, *)
  func select(withFrame aRect: NSRect, editor textObj: NSText, delegate anObject: AnyObject?, start selStart: Int, length selLength: Int)
  @available(OSX 10.10, *)
  func endEditing(_ textObj: NSText)
}
extension NSObject {
  class func controlTextDidBeginEditing(_ obj: NSNotification)
  func controlTextDidBeginEditing(_ obj: NSNotification)
  class func controlTextDidEndEditing(_ obj: NSNotification)
  func controlTextDidEndEditing(_ obj: NSNotification)
  class func controlTextDidChange(_ obj: NSNotification)
  func controlTextDidChange(_ obj: NSNotification)
}
protocol NSControlTextEditingDelegate : NSObjectProtocol {
  @discardableResult
  optional func control(_ control: NSControl, textShouldBeginEditing fieldEditor: NSText) -> Bool
  @discardableResult
  optional func control(_ control: NSControl, textShouldEndEditing fieldEditor: NSText) -> Bool
  @discardableResult
  optional func control(_ control: NSControl, didFailToFormatString string: String, errorDescription error: String?) -> Bool
  optional func control(_ control: NSControl, didFailToValidatePartialString string: String, errorDescription error: String?)
  @discardableResult
  optional func control(_ control: NSControl, isValidObject obj: AnyObject) -> Bool
  @discardableResult
  optional func control(_ control: NSControl, textView textView: NSTextView, doCommandBy commandSelector: Selector) -> Bool
  @discardableResult
  optional func control(_ control: NSControl, textView textView: NSTextView, completions words: [String], forPartialWordRange charRange: NSRange, indexOfSelectedItem index: UnsafeMutablePointer<Int>) -> [String]
}
let NSControlTextDidBeginEditingNotification: String
let NSControlTextDidEndEditingNotification: String
let NSControlTextDidChangeNotification: String
extension NSControl {
  class func setCellClass(_ factoryId: AnyClass?)
  @discardableResult
  class func cellClass() -> AnyClass?
  var cell: NSCell?
  @discardableResult
  func selectedCell() -> NSCell?
  @discardableResult
  func selectedTag() -> Int
  func setNeedsDisplay()
  func calcSize()
  func updateCell(_ aCell: NSCell)
  func updateCell(inside aCell: NSCell)
  func drawCell(inside aCell: NSCell)
  func drawCell(_ aCell: NSCell)
  func selectCell(_ aCell: NSCell)
}
