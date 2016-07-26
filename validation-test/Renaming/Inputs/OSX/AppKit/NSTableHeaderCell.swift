
class NSTableHeaderCell : NSTextFieldCell {
  func drawSortIndicator(withFrame cellFrame: NSRect, in controlView: NSView, ascending ascending: Bool, priority priority: Int)
  @discardableResult
  func sortIndicatorRect(forBounds theRect: NSRect) -> NSRect
}
