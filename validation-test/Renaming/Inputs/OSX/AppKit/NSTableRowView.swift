
@available(OSX 10.7, *)
class NSTableRowView : NSView, NSAccessibilityRow {
  var selectionHighlightStyle: NSTableViewSelectionHighlightStyle
  var isEmphasized: Bool
  var isGroupRowStyle: Bool
  var isSelected: Bool
  @available(OSX 10.10, *)
  var isPreviousRowSelected: Bool
  var isNextRowSelected: Bool
  var isFloating: Bool
  var isTargetForDropOperation: Bool
  var draggingDestinationFeedbackStyle: NSTableViewDraggingDestinationFeedbackStyle
  var indentationForDropOperation: CGFloat
  var interiorBackgroundStyle: NSBackgroundStyle { get }
  @NSCopying var backgroundColor: NSColor
  func drawBackground(in dirtyRect: NSRect)
  func drawSelection(in dirtyRect: NSRect)
  func drawSeparator(in dirtyRect: NSRect)
  func drawDraggingDestinationFeedback(in dirtyRect: NSRect)
  @discardableResult
  func view(atColumn column: Int) -> AnyObject?
  var numberOfColumns: Int { get }
}
