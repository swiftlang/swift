
enum NSMatrixMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case radioModeMatrix
  case highlightModeMatrix
  case listModeMatrix
  case trackModeMatrix
}
struct __MFlags {
  var reservedMatrix: UInt32
  var cellSizeNeedsAutorecalc: UInt32
  var autorecalculatesCellSize: UInt32
  var drawingContextMenuHighlightOnAllSelectedRows: UInt32
  var drawingContextMenuHighlight: UInt32
  var browserOptimizationsEnabled: UInt32
  var needsRedrawBeforeFirstLiveResizeCache: UInt32
  var tmpAllowNonVisibleCellsToBecomeFirstResponder: UInt32
  var subclassIsSafeForLiveResize: UInt32
  var hasCachedSubclassIsSafeForLiveResize: UInt32
  var liveResizeImageCacheingEnabled: UInt32
  var checkForSimpleTrackingMode: UInt32
  var useSimpleTrackingMode: UInt32
  var refusesFirstResponder: UInt32
  var dontScroll: UInt32
  var changingSelectionWithKeyboard: UInt32
  var onlySetKeyCell: UInt32
  var currentlySelectingCell: UInt32
  var allowsIncrementalSearching: UInt32
  var tabKeyTraversesCellsExplicitlySet: UInt32
  var tabKeyTraversesCells: UInt32
  var drawingAncestor: UInt32
  var autosizeCells: UInt32
  var drawsBackground: UInt32
  var drawsCellBackground: UInt32
  var selectionByRect: UInt32
  var autoscroll: UInt32
  var allowEmptySel: UInt32
  var listMode: UInt32
  var radioMode: UInt32
  var highlightMode: UInt32
  init()
  init(reservedMatrix reservedMatrix: UInt32, cellSizeNeedsAutorecalc cellSizeNeedsAutorecalc: UInt32, autorecalculatesCellSize autorecalculatesCellSize: UInt32, drawingContextMenuHighlightOnAllSelectedRows drawingContextMenuHighlightOnAllSelectedRows: UInt32, drawingContextMenuHighlight drawingContextMenuHighlight: UInt32, browserOptimizationsEnabled browserOptimizationsEnabled: UInt32, needsRedrawBeforeFirstLiveResizeCache needsRedrawBeforeFirstLiveResizeCache: UInt32, tmpAllowNonVisibleCellsToBecomeFirstResponder tmpAllowNonVisibleCellsToBecomeFirstResponder: UInt32, subclassIsSafeForLiveResize subclassIsSafeForLiveResize: UInt32, hasCachedSubclassIsSafeForLiveResize hasCachedSubclassIsSafeForLiveResize: UInt32, liveResizeImageCacheingEnabled liveResizeImageCacheingEnabled: UInt32, checkForSimpleTrackingMode checkForSimpleTrackingMode: UInt32, useSimpleTrackingMode useSimpleTrackingMode: UInt32, refusesFirstResponder refusesFirstResponder: UInt32, dontScroll dontScroll: UInt32, changingSelectionWithKeyboard changingSelectionWithKeyboard: UInt32, onlySetKeyCell onlySetKeyCell: UInt32, currentlySelectingCell currentlySelectingCell: UInt32, allowsIncrementalSearching allowsIncrementalSearching: UInt32, tabKeyTraversesCellsExplicitlySet tabKeyTraversesCellsExplicitlySet: UInt32, tabKeyTraversesCells tabKeyTraversesCells: UInt32, drawingAncestor drawingAncestor: UInt32, autosizeCells autosizeCells: UInt32, drawsBackground drawsBackground: UInt32, drawsCellBackground drawsCellBackground: UInt32, selectionByRect selectionByRect: UInt32, autoscroll autoscroll: UInt32, allowEmptySel allowEmptySel: UInt32, listMode listMode: UInt32, radioMode radioMode: UInt32, highlightMode highlightMode: UInt32)
}
typealias _MFlags = __MFlags
class NSMatrix : NSControl, NSUserInterfaceValidations {
  init(frame frameRect: NSRect, mode aMode: NSMatrixMode, prototype aCell: NSCell, numberOfRows rowsHigh: Int, numberOfColumns colsWide: Int)
  init(frame frameRect: NSRect, mode aMode: NSMatrixMode, cellClass factoryId: AnyClass?, numberOfRows rowsHigh: Int, numberOfColumns colsWide: Int)
  var cellClass: AnyClass
  @NSCopying var prototype: NSCell?
  @discardableResult
  func makeCell(atRow row: Int, column col: Int) -> NSCell
  var mode: NSMatrixMode
  var allowsEmptySelection: Bool
  func sendAction(_ aSelector: Selector, to anObject: AnyObject, forAllCells flag: Bool)
  var cells: [NSCell] { get }
  func sort(using comparator: Selector)
  func sort(_ compare: @convention(c) (AnyObject, AnyObject, UnsafeMutablePointer<Void>?) -> Int, context context: UnsafeMutablePointer<Void>?)
  var selectedCells: [NSCell] { get }
  var selectedRow: Int { get }
  var selectedColumn: Int { get }
  var isSelectionByRect: Bool
  func setSelectionFrom(_ startPos: Int, to endPos: Int, anchor anchorPos: Int, highlight lit: Bool)
  func deselectSelectedCell()
  func deselectAllCells()
  func selectCell(atRow row: Int, column col: Int)
  @discardableResult
  func selectCell(withTag anInt: Int) -> Bool
  var cellSize: NSSize
  var intercellSpacing: NSSize
  func setScrollable(_ flag: Bool)
  @NSCopying var backgroundColor: NSColor
  @NSCopying var cellBackgroundColor: NSColor?
  var drawsCellBackground: Bool
  var drawsBackground: Bool
  func setState(_ value: Int, atRow row: Int, column col: Int)
  func getNumberOfRows(_ rowCount: UnsafeMutablePointer<Int>?, columns colCount: UnsafeMutablePointer<Int>?)
  var numberOfRows: Int { get }
  var numberOfColumns: Int { get }
  @discardableResult
  func cell(atRow row: Int, column col: Int) -> NSCell?
  @discardableResult
  func cellFrame(atRow row: Int, column col: Int) -> NSRect
  @discardableResult
  func getRow(_ row: UnsafeMutablePointer<Int>, column col: UnsafeMutablePointer<Int>, of aCell: NSCell) -> Bool
  @discardableResult
  func getRow(_ row: UnsafeMutablePointer<Int>, column col: UnsafeMutablePointer<Int>, for aPoint: NSPoint) -> Bool
  func renewRows(_ newRows: Int, columns newCols: Int)
  func putCell(_ newCell: NSCell, atRow row: Int, column col: Int)
  func addRow()
  func addRow(with newCells: [NSCell])
  func insertRow(_ row: Int)
  func insertRow(_ row: Int, with newCells: [NSCell])
  func removeRow(_ row: Int)
  func addColumn()
  func addColumn(with newCells: [NSCell])
  func insertColumn(_ column: Int)
  func insertColumn(_ column: Int, with newCells: [NSCell])
  func removeColumn(_ col: Int)
  @discardableResult
  func cell(withTag anInt: Int) -> NSCell?
  var doubleAction: Selector?
  var autosizesCells: Bool
  func sizeToCells()
  func setValidateSize(_ flag: Bool)
  func drawCell(atRow row: Int, column col: Int)
  func highlightCell(_ flag: Bool, atRow row: Int, column col: Int)
  var isAutoscroll: Bool
  func scrollCellToVisible(atRow row: Int, column col: Int)
  var mouseDownFlags: Int { get }
  @discardableResult
  func sendAction() -> Bool
  func sendDoubleAction()
  unowned(unsafe) var delegate: @sil_unmanaged NSMatrixDelegate?
  @discardableResult
  func textShouldBeginEditing(_ textObject: NSText) -> Bool
  @discardableResult
  func textShouldEndEditing(_ textObject: NSText) -> Bool
  func textDidBeginEditing(_ notification: NSNotification)
  func textDidEndEditing(_ notification: NSNotification)
  func textDidChange(_ notification: NSNotification)
  func selectText(_ sender: AnyObject?)
  @discardableResult
  func selectText(atRow row: Int, column col: Int) -> NSCell?
  func setToolTip(_ toolTipString: String?, for cell: NSCell)
  @discardableResult
  func toolTip(for cell: NSCell) -> String?
  @available(OSX 10.8, *)
  var autorecalculatesCellSize: Bool
}
extension NSMatrix {
  var tabKeyTraversesCells: Bool
  unowned(unsafe) var keyCell: @sil_unmanaged NSCell?
}
protocol NSMatrixDelegate : NSControlTextEditingDelegate {
}
