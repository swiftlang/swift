
protocol NSTextInputClient {
  func insertText(_ aString: AnyObject, replacementRange replacementRange: NSRange)
  func doCommand(by aSelector: Selector)
  func setMarkedText(_ aString: AnyObject, selectedRange selectedRange: NSRange, replacementRange replacementRange: NSRange)
  func unmarkText()
  @discardableResult
  func selectedRange() -> NSRange
  @discardableResult
  func markedRange() -> NSRange
  @discardableResult
  func hasMarkedText() -> Bool
  @available(OSX 10.0, *)
  @discardableResult
  func attributedSubstring(forProposedRange aRange: NSRange, actualRange actualRange: NSRangePointer?) -> NSAttributedString?
  @discardableResult
  func validAttributesForMarkedText() -> [String]
  @discardableResult
  func firstRect(forCharacterRange aRange: NSRange, actualRange actualRange: NSRangePointer?) -> NSRect
  @discardableResult
  func characterIndex(for aPoint: NSPoint) -> Int
  @available(OSX 10.0, *)
  @discardableResult
  optional func attributedString() -> NSAttributedString
  @discardableResult
  optional func fractionOfDistanceThroughGlyph(for aPoint: NSPoint) -> CGFloat
  @discardableResult
  optional func baselineDeltaForCharacter(at anIndex: Int) -> CGFloat
  @discardableResult
  optional func windowLevel() -> Int
  @available(OSX 10.6, *)
  @discardableResult
  optional func drawsVerticallyForCharacter(at charIndex: Int) -> Bool
}
