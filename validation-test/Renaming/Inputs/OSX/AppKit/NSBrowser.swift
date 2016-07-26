
var NSAppKitVersionNumberWithContinuousScrollingBrowser: Double { get }
var NSAppKitVersionNumberWithColumnResizingBrowser: Double { get }
struct __Brflags {
  var firstVisibleCalculationDisabled: UInt32
  var prefersAllColumnUserResizing: UInt32
  var usesSmallScrollers: UInt32
  var usesSmallSizeTitleFont: UInt32
  var actionNeedsToBeSent: UInt32
  var acceptsFirstMouse: UInt32
  var refusesFirstResponder: UInt32
  var disableCompositing: UInt32
  var delegateSelectsCellsByRow: UInt32
  var allowsIncrementalSearching: UInt32
  var time: UInt32
  var hasHorizontalScroller: UInt32
  var prohibitEmptySel: UInt32
  var sendActionOnArrowKeys: UInt32
  var dontDrawTitles: UInt32
  var acceptArrowKeys: UInt32
  var delegateValidatesColumns: UInt32
  var delegateDoesNotCreateRowsInMatrix: UInt32
  var delegateSelectsCellsByString: UInt32
  var delegateSetsTitles: UInt32
  var delegateImplementsWillDisplayCell: UInt32
  var separateColumns: UInt32
  var titleFromPrevious: UInt32
  var isTitled: UInt32
  var reuseColumns: UInt32
  var allowsBranchSelection: UInt32
  var allowsMultipleSelection: UInt32
  init()
  init(firstVisibleCalculationDisabled firstVisibleCalculationDisabled: UInt32, prefersAllColumnUserResizing prefersAllColumnUserResizing: UInt32, usesSmallScrollers usesSmallScrollers: UInt32, usesSmallSizeTitleFont usesSmallSizeTitleFont: UInt32, actionNeedsToBeSent actionNeedsToBeSent: UInt32, acceptsFirstMouse acceptsFirstMouse: UInt32, refusesFirstResponder refusesFirstResponder: UInt32, disableCompositing disableCompositing: UInt32, delegateSelectsCellsByRow delegateSelectsCellsByRow: UInt32, allowsIncrementalSearching allowsIncrementalSearching: UInt32, time time: UInt32, hasHorizontalScroller hasHorizontalScroller: UInt32, prohibitEmptySel prohibitEmptySel: UInt32, sendActionOnArrowKeys sendActionOnArrowKeys: UInt32, dontDrawTitles dontDrawTitles: UInt32, acceptArrowKeys acceptArrowKeys: UInt32, delegateValidatesColumns delegateValidatesColumns: UInt32, delegateDoesNotCreateRowsInMatrix delegateDoesNotCreateRowsInMatrix: UInt32, delegateSelectsCellsByString delegateSelectsCellsByString: UInt32, delegateSetsTitles delegateSetsTitles: UInt32, delegateImplementsWillDisplayCell delegateImplementsWillDisplayCell: UInt32, separateColumns separateColumns: UInt32, titleFromPrevious titleFromPrevious: UInt32, isTitled isTitled: UInt32, reuseColumns reuseColumns: UInt32, allowsBranchSelection allowsBranchSelection: UInt32, allowsMultipleSelection allowsMultipleSelection: UInt32)
}
typealias _Brflags = __Brflags
enum NSBrowserColumnResizingType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noColumnResizing
  case autoColumnResizing
  case userColumnResizing
}
@available(OSX 10.5, *)
enum NSBrowserDropOperation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case on
  case above
}
class NSBrowser : NSControl {
  func loadColumnZero()
  var isLoaded: Bool { get }
  var doubleAction: Selector?
  func setCellClass(_ factoryId: AnyClass)
  var cellPrototype: AnyObject!
  unowned(unsafe) var delegate: @sil_unmanaged NSBrowserDelegate?
  var reusesColumns: Bool
  var hasHorizontalScroller: Bool
  @available(OSX 10.6, *)
  var autohidesScroller: Bool
  var separatesColumns: Bool
  var isTitled: Bool
  var minColumnWidth: CGFloat
  var maxVisibleColumns: Int
  var allowsMultipleSelection: Bool
  var allowsBranchSelection: Bool
  var allowsEmptySelection: Bool
  var takesTitleFromPreviousColumn: Bool
  var sendsActionOnArrowKeys: Bool
  @available(OSX 10.6, *)
  @discardableResult
  func item(at indexPath: NSIndexPath) -> AnyObject?
  @available(OSX 10.6, *)
  @discardableResult
  func item(atRow row: Int, inColumn column: Int) -> AnyObject?
  @available(OSX 10.6, *)
  @discardableResult
  func indexPath(forColumn column: Int) -> NSIndexPath
  @available(OSX 10.6, *)
  @discardableResult
  func isLeafItem(_ item: AnyObject?) -> Bool
  @available(OSX 10.6, *)
  func reloadData(forRowIndexes rowIndexes: NSIndexSet, inColumn column: Int)
  @available(OSX 10.6, *)
  @discardableResult
  func parentForItems(inColumn column: Int) -> AnyObject?
  @available(OSX 10.6, *)
  func scrollRowToVisible(_ row: Int, inColumn column: Int)
  func setTitle(_ aString: String, ofColumn column: Int)
  @discardableResult
  func title(ofColumn column: Int) -> String?
  var pathSeparator: String
  @discardableResult
  func setPath(_ path: String) -> Bool
  @discardableResult
  func path() -> String
  @discardableResult
  func path(toColumn column: Int) -> String
  @available(OSX 10.6, *)
  var clickedColumn: Int { get }
  @available(OSX 10.6, *)
  var clickedRow: Int { get }
  var selectedColumn: Int { get }
  @discardableResult
  func selectedCell(inColumn column: Int) -> AnyObject?
  var selectedCells: [NSCell]? { get }
  func selectRow(_ row: Int, inColumn column: Int)
  @discardableResult
  func selectedRow(inColumn column: Int) -> Int
  @available(OSX 10.6, *)
  @NSCopying var selectionIndexPath: NSIndexPath
  @available(OSX 10.6, *)
  var selectionIndexPaths: [NSIndexPath]
  @available(OSX 10.5, *)
  func selectRowIndexes(_ indexes: NSIndexSet, inColumn column: Int)
  @available(OSX 10.5, *)
  @discardableResult
  func selectedRowIndexes(inColumn column: Int) -> NSIndexSet?
  func reloadColumn(_ column: Int)
  func validateVisibleColumns()
  func scrollColumnsRight(by shiftAmount: Int)
  func scrollColumnsLeft(by shiftAmount: Int)
  func scrollColumnToVisible(_ column: Int)
  var lastColumn: Int
  func addColumn()
  var numberOfVisibleColumns: Int { get }
  var firstVisibleColumn: Int { get }
  var lastVisibleColumn: Int { get }
  @discardableResult
  func loadedCell(atRow row: Int, column col: Int) -> AnyObject?
  func tile()
  func doClick(_ sender: AnyObject?)
  func doDoubleClick(_ sender: AnyObject?)
  @discardableResult
  func sendAction() -> Bool
  @discardableResult
  func titleFrame(ofColumn column: Int) -> NSRect
  func drawTitle(ofColumn column: Int, in aRect: NSRect)
  var titleHeight: CGFloat { get }
  @discardableResult
  func frame(ofColumn column: Int) -> NSRect
  @discardableResult
  func frameOf(insideOfColumn column: Int) -> NSRect
  @available(OSX 10.6, *)
  @discardableResult
  func frame(ofRow row: Int, inColumn column: Int) -> NSRect
  @available(OSX 10.6, *)
  @discardableResult
  func getRow(_ row: UnsafeMutablePointer<Int>?, column column: UnsafeMutablePointer<Int>?, for point: NSPoint) -> Bool
  @discardableResult
  func columnWidth(forColumnContentWidth columnContentWidth: CGFloat) -> CGFloat
  @discardableResult
  func columnContentWidth(forColumnWidth columnWidth: CGFloat) -> CGFloat
  var columnResizingType: NSBrowserColumnResizingType
  var prefersAllColumnUserResizing: Bool
  func setWidth(_ columnWidth: CGFloat, ofColumn columnIndex: Int)
  @discardableResult
  func width(ofColumn column: Int) -> CGFloat
  @available(OSX 10.6, *)
  var rowHeight: CGFloat
  @available(OSX 10.6, *)
  func noteHeightOfRows(withIndexesChanged indexSet: NSIndexSet, inColumn columnIndex: Int)
  @available(OSX 10.6, *)
  func setDefaultColumnWidth(_ columnWidth: CGFloat)
  @available(OSX 10.6, *)
  @discardableResult
  func defaultColumnWidth() -> CGFloat
  var columnsAutosaveName: String
  class func removeSavedColumns(withAutosaveName name: String)
  @available(OSX 10.5, *)
  @discardableResult
  func canDragRows(with rowIndexes: NSIndexSet, inColumn column: Int, with event: NSEvent) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func draggingImageForRows(with rowIndexes: NSIndexSet, inColumn column: Int, with event: NSEvent, offset dragImageOffset: NSPointPointer?) -> NSImage?
  @available(OSX 10.5, *)
  func setDraggingSourceOperationMask(_ mask: NSDragOperation, forLocal isLocal: Bool)
  @available(OSX 10.5, *)
  var allowsTypeSelect: Bool
  @available(OSX 10.5, *)
  var backgroundColor: NSColor
  @available(OSX 10.6, *)
  func editItem(at indexPath: NSIndexPath, with theEvent: NSEvent, select select: Bool)
}
let NSBrowserColumnConfigurationDidChangeNotification: String
protocol NSBrowserDelegate : NSObjectProtocol {
  @discardableResult
  optional func browser(_ sender: NSBrowser, numberOfRowsInColumn column: Int) -> Int
  optional func browser(_ sender: NSBrowser, createRowsForColumn column: Int, in matrix: NSMatrix)
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, numberOfChildrenOfItem item: AnyObject?) -> Int
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, child index: Int, ofItem item: AnyObject?) -> AnyObject
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, isLeafItem item: AnyObject?) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, objectValueForItem item: AnyObject?) -> AnyObject?
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, heightOfRow row: Int, inColumn columnIndex: Int) -> CGFloat
  @available(OSX 10.6, *)
  @discardableResult
  optional func rootItem(for browser: NSBrowser) -> AnyObject?
  @available(OSX 10.6, *)
  optional func browser(_ browser: NSBrowser, setObjectValue object: AnyObject?, forItem item: AnyObject?)
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, shouldEditItem item: AnyObject?) -> Bool
  optional func browser(_ sender: NSBrowser, willDisplayCell cell: AnyObject, atRow row: Int, column column: Int)
  @discardableResult
  optional func browser(_ sender: NSBrowser, titleOfColumn column: Int) -> String?
  @discardableResult
  optional func browser(_ sender: NSBrowser, selectCellWith title: String, inColumn column: Int) -> Bool
  @discardableResult
  optional func browser(_ sender: NSBrowser, selectRow row: Int, inColumn column: Int) -> Bool
  @discardableResult
  optional func browser(_ sender: NSBrowser, isColumnValid column: Int) -> Bool
  optional func browserWillScroll(_ sender: NSBrowser)
  optional func browserDidScroll(_ sender: NSBrowser)
  @discardableResult
  optional func browser(_ browser: NSBrowser, shouldSizeColumn columnIndex: Int, forUserResize forUserResize: Bool, toWidth suggestedWidth: CGFloat) -> CGFloat
  @discardableResult
  optional func browser(_ browser: NSBrowser, sizeToFitWidthOfColumn columnIndex: Int) -> CGFloat
  optional func browserColumnConfigurationDidChange(_ notification: NSNotification)
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, shouldShowCellExpansionForRow row: Int, column column: Int) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, writeRowsWith rowIndexes: NSIndexSet, inColumn column: Int, to pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, namesOfPromisedFilesDroppedAtDestination dropDestination: NSURL, forDraggedRowsWith rowIndexes: NSIndexSet, inColumn column: Int) -> [String]
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, canDragRowsWith rowIndexes: NSIndexSet, inColumn column: Int, with event: NSEvent) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, draggingImageForRowsWith rowIndexes: NSIndexSet, inColumn column: Int, with event: NSEvent, offset dragImageOffset: NSPointPointer) -> NSImage?
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, validateDrop info: NSDraggingInfo, proposedRow row: UnsafeMutablePointer<Int>, column column: UnsafeMutablePointer<Int>, dropOperation dropOperation: UnsafeMutablePointer<NSBrowserDropOperation>) -> NSDragOperation
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, acceptDrop info: NSDraggingInfo, atRow row: Int, column column: Int, dropOperation dropOperation: NSBrowserDropOperation) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, typeSelectStringForRow row: Int, inColumn column: Int) -> String
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, shouldTypeSelectFor event: NSEvent, withCurrentSearch searchString: String?) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, nextTypeSelectMatchFromRow startRow: Int, toRow endRow: Int, inColumn column: Int, for searchString: String?) -> Int
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, previewViewControllerForLeafItem item: AnyObject) -> NSViewController?
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, headerViewControllerForItem item: AnyObject?) -> NSViewController?
  optional func browser(_ browser: NSBrowser, didChangeLastColumn oldLastColumn: Int, toColumn column: Int)
  @available(OSX 10.6, *)
  @discardableResult
  optional func browser(_ browser: NSBrowser, selectionIndexesForProposedSelection proposedSelectionIndexes: NSIndexSet, inColumn column: Int) -> NSIndexSet
}
extension NSBrowser {
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use the item based NSBrowser instead")
  func setMatrixClass(_ factoryId: AnyClass)
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use the item based NSBrowser instead")
  @discardableResult
  func matrixClass() -> AnyClass
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use the item based NSBrowser instead")
  @discardableResult
  func column(of matrix: NSMatrix) -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use the item based NSBrowser instead")
  @discardableResult
  func matrix(inColumn column: Int) -> NSMatrix?
}
