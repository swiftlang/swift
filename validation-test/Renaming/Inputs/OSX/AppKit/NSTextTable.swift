
enum NSTextBlockValueType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case absoluteValueType
  case percentageValueType
}
enum NSTextBlockDimension : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case width
  case minimumWidth
  case maximumWidth
  case height
  case minimumHeight
  case maximumHeight
}
enum NSTextBlockLayer : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case padding
  case border
  case margin
}
enum NSTextBlockVerticalAlignment : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case topAlignment
  case middleAlignment
  case bottomAlignment
  case baselineAlignment
}
enum NSTextTableLayoutAlgorithm : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case automaticLayoutAlgorithm
  case fixedLayoutAlgorithm
}
class NSTextBlock : NSObject, NSCoding, NSCopying {
  func setValue(_ val: CGFloat, type type: NSTextBlockValueType, for dimension: NSTextBlockDimension)
  @discardableResult
  func value(for dimension: NSTextBlockDimension) -> CGFloat
  @discardableResult
  func valueType(for dimension: NSTextBlockDimension) -> NSTextBlockValueType
  func setContentWidth(_ val: CGFloat, type type: NSTextBlockValueType)
  var contentWidth: CGFloat { get }
  var contentWidthValueType: NSTextBlockValueType { get }
  func setWidth(_ val: CGFloat, type type: NSTextBlockValueType, for layer: NSTextBlockLayer, edge edge: NSRectEdge)
  func setWidth(_ val: CGFloat, type type: NSTextBlockValueType, for layer: NSTextBlockLayer)
  @discardableResult
  func width(for layer: NSTextBlockLayer, edge edge: NSRectEdge) -> CGFloat
  @discardableResult
  func widthValueType(for layer: NSTextBlockLayer, edge edge: NSRectEdge) -> NSTextBlockValueType
  var verticalAlignment: NSTextBlockVerticalAlignment
  @NSCopying var backgroundColor: NSColor?
  func setBorderColor(_ color: NSColor?, for edge: NSRectEdge)
  func setBorderColor(_ color: NSColor?)
  @discardableResult
  func borderColor(for edge: NSRectEdge) -> NSColor?
  @discardableResult
  func rectForLayout(at startingPoint: NSPoint, in rect: NSRect, textContainer textContainer: NSTextContainer, characterRange charRange: NSRange) -> NSRect
  @discardableResult
  func boundsRect(forContentRect contentRect: NSRect, in rect: NSRect, textContainer textContainer: NSTextContainer, characterRange charRange: NSRange) -> NSRect
  func drawBackground(withFrame frameRect: NSRect, in controlView: NSView, characterRange charRange: NSRange, layoutManager layoutManager: NSLayoutManager)
}
class NSTextTableBlock : NSTextBlock {
  init(table table: NSTextTable, startingRow row: Int, rowSpan rowSpan: Int, startingColumn col: Int, columnSpan colSpan: Int)
  var table: NSTextTable { get }
  var startingRow: Int { get }
  var rowSpan: Int { get }
  var startingColumn: Int { get }
  var columnSpan: Int { get }
}
class NSTextTable : NSTextBlock {
  var numberOfColumns: Int
  var layoutAlgorithm: NSTextTableLayoutAlgorithm
  var collapsesBorders: Bool
  var hidesEmptyCells: Bool
  @discardableResult
  func rect(for block: NSTextTableBlock, layoutAt startingPoint: NSPoint, in rect: NSRect, textContainer textContainer: NSTextContainer, characterRange charRange: NSRange) -> NSRect
  @discardableResult
  func boundsRect(for block: NSTextTableBlock, contentRect contentRect: NSRect, in rect: NSRect, textContainer textContainer: NSTextContainer, characterRange charRange: NSRange) -> NSRect
  func drawBackground(for block: NSTextTableBlock, withFrame frameRect: NSRect, in controlView: NSView, characterRange charRange: NSRange, layoutManager layoutManager: NSLayoutManager)
}
