
class NSFormCell : NSActionCell {
  @discardableResult
  func titleWidth(_ aSize: NSSize) -> CGFloat
  var titleWidth: CGFloat
  var titleFont: NSFont
  var titleAlignment: NSTextAlignment
  var placeholderString: String?
  @NSCopying var placeholderAttributedString: NSAttributedString?
  var titleBaseWritingDirection: NSWritingDirection
  @available(OSX 10.8, *)
  var preferredTextFieldWidth: CGFloat
}
extension NSFormCell {
}
extension NSFormCell {
  @NSCopying var attributedTitle: NSAttributedString
}
