
struct __OvFlags {
  var allItemsLoaded: UInt32
  var dataSourceObjectValueByItem: UInt32
  var allowAutomaticAnimations: UInt32
  var dontRedisplayOnFrameChange: UInt32
  var _isSpringLoadingFlashing: UInt32
  var unused2: UInt32
  var delegateShouldAutoExpandItem: UInt32
  var delegateAutoCollapseItem: UInt32
  var delegateAutoExpandItem: UInt32
  var delegateShouldShowOutlineCellForItem: UInt32
  var dataSourceDraggedImageMovedTo: UInt32
  var dataSourceDraggingEndedAt: UInt32
  var reloadingData: UInt32
  var validDataSourceMethods: UInt32
  var numberOfRowsDataExpandEntered: UInt32
  var delayRowEntryFreeDisabled: UInt32
  var delegateHeightOfRowByItem: UInt32
  var animateExpandAndCollapse: UInt32
  var unused: UInt32
  var selectionAdjustmentDisabled: UInt32
  var subclassRowForItem: UInt32
  var delegateWillDisplayOutlineCell: UInt32
  var enableExpandNotifications: UInt32
  var autoSaveExpandItems: UInt32
  var autoresizesOutlineColumn: UInt32
  var delegateShouldExpandItem: UInt32
  var delegateShouldCollapseItem: UInt32
  var delegateSelectionShouldChangeInOutlineView: UInt32
  var delegateShouldSelectTableColumn: UInt32
  var delegateShouldSelectItem: UInt32
  var delegateShouldEditTableColumn: UInt32
  var delegateWillDisplayCell: UInt32
  init()
  init(allItemsLoaded allItemsLoaded: UInt32, dataSourceObjectValueByItem dataSourceObjectValueByItem: UInt32, allowAutomaticAnimations allowAutomaticAnimations: UInt32, dontRedisplayOnFrameChange dontRedisplayOnFrameChange: UInt32, _isSpringLoadingFlashing _isSpringLoadingFlashing: UInt32, unused2 unused2: UInt32, delegateShouldAutoExpandItem delegateShouldAutoExpandItem: UInt32, delegateAutoCollapseItem delegateAutoCollapseItem: UInt32, delegateAutoExpandItem delegateAutoExpandItem: UInt32, delegateShouldShowOutlineCellForItem delegateShouldShowOutlineCellForItem: UInt32, dataSourceDraggedImageMovedTo dataSourceDraggedImageMovedTo: UInt32, dataSourceDraggingEndedAt dataSourceDraggingEndedAt: UInt32, reloadingData reloadingData: UInt32, validDataSourceMethods validDataSourceMethods: UInt32, numberOfRowsDataExpandEntered numberOfRowsDataExpandEntered: UInt32, delayRowEntryFreeDisabled delayRowEntryFreeDisabled: UInt32, delegateHeightOfRowByItem delegateHeightOfRowByItem: UInt32, animateExpandAndCollapse animateExpandAndCollapse: UInt32, unused unused: UInt32, selectionAdjustmentDisabled selectionAdjustmentDisabled: UInt32, subclassRowForItem subclassRowForItem: UInt32, delegateWillDisplayOutlineCell delegateWillDisplayOutlineCell: UInt32, enableExpandNotifications enableExpandNotifications: UInt32, autoSaveExpandItems autoSaveExpandItems: UInt32, autoresizesOutlineColumn autoresizesOutlineColumn: UInt32, delegateShouldExpandItem delegateShouldExpandItem: UInt32, delegateShouldCollapseItem delegateShouldCollapseItem: UInt32, delegateSelectionShouldChangeInOutlineView delegateSelectionShouldChangeInOutlineView: UInt32, delegateShouldSelectTableColumn delegateShouldSelectTableColumn: UInt32, delegateShouldSelectItem delegateShouldSelectItem: UInt32, delegateShouldEditTableColumn delegateShouldEditTableColumn: UInt32, delegateWillDisplayCell delegateWillDisplayCell: UInt32)
}
typealias _OVFlags = __OvFlags
var NSOutlineViewDropOnItemIndex: Int { get }
class NSOutlineView : NSTableView, NSAccessibilityOutline {
  unowned(unsafe) var outlineTableColumn: @sil_unmanaged NSTableColumn?
  @discardableResult
  func isExpandable(_ item: AnyObject?) -> Bool
  @available(OSX 10.10, *)
  @discardableResult
  func numberOfChildren(ofItem item: AnyObject?) -> Int
  @available(OSX 10.10, *)
  @discardableResult
  func child(_ index: Int, ofItem item: AnyObject?) -> AnyObject?
  func expandItem(_ item: AnyObject?, expandChildren expandChildren: Bool)
  func expandItem(_ item: AnyObject?)
  func collapseItem(_ item: AnyObject?, collapseChildren collapseChildren: Bool)
  func collapseItem(_ item: AnyObject?)
  func reloadItem(_ item: AnyObject?, reloadChildren reloadChildren: Bool)
  func reloadItem(_ item: AnyObject?)
  @discardableResult
  func parent(forItem item: AnyObject?) -> AnyObject?
  @available(OSX 10.11, *)
  @discardableResult
  func childIndex(forItem item: AnyObject) -> Int
  @discardableResult
  func item(atRow row: Int) -> AnyObject?
  @discardableResult
  func row(forItem item: AnyObject?) -> Int
  @discardableResult
  func level(forItem item: AnyObject?) -> Int
  @discardableResult
  func level(forRow row: Int) -> Int
  @discardableResult
  func isItemExpanded(_ item: AnyObject?) -> Bool
  var indentationPerLevel: CGFloat
  var indentationMarkerFollowsCell: Bool
  var autoresizesOutlineColumn: Bool
  @available(OSX 10.5, *)
  @discardableResult
  func frameOfOutlineCell(atRow row: Int) -> NSRect
  func setDropItem(_ item: AnyObject?, dropChildIndex index: Int)
  @discardableResult
  func shouldCollapseAutoExpandedItems(forDeposited deposited: Bool) -> Bool
  var autosaveExpandedItems: Bool
  @available(OSX 10.7, *)
  func insertItems(at indexes: NSIndexSet, inParent parent: AnyObject?, withAnimation animationOptions: NSTableViewAnimationOptions = [])
  @available(OSX 10.7, *)
  func removeItems(at indexes: NSIndexSet, inParent parent: AnyObject?, withAnimation animationOptions: NSTableViewAnimationOptions = [])
  @available(OSX 10.7, *)
  func moveItem(at fromIndex: Int, inParent oldParent: AnyObject?, to toIndex: Int, inParent newParent: AnyObject?)
}
protocol NSOutlineViewDataSource : NSObjectProtocol {
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, numberOfChildrenOfItem item: AnyObject?) -> Int
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, child index: Int, ofItem item: AnyObject?) -> AnyObject
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, isItemExpandable item: AnyObject) -> Bool
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, objectValueFor tableColumn: NSTableColumn?, byItem item: AnyObject?) -> AnyObject?
  optional func outlineView(_ outlineView: NSOutlineView, setObjectValue object: AnyObject?, for tableColumn: NSTableColumn?, byItem item: AnyObject?)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, itemForPersistentObject object: AnyObject) -> AnyObject?
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, persistentObjectForItem item: AnyObject?) -> AnyObject?
  optional func outlineView(_ outlineView: NSOutlineView, sortDescriptorsDidChange oldDescriptors: [NSSortDescriptor])
  @available(OSX 10.7, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, pasteboardWriterForItem item: AnyObject) -> NSPasteboardWriting?
  @available(OSX 10.7, *)
  optional func outlineView(_ outlineView: NSOutlineView, draggingSession session: NSDraggingSession, willBeginAt screenPoint: NSPoint, forItems draggedItems: [AnyObject])
  @available(OSX 10.7, *)
  optional func outlineView(_ outlineView: NSOutlineView, draggingSession session: NSDraggingSession, endedAt screenPoint: NSPoint, operation operation: NSDragOperation)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, writeItems items: [AnyObject], to pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.7, *)
  optional func outlineView(_ outlineView: NSOutlineView, updateDraggingItemsForDrag draggingInfo: NSDraggingInfo)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, validateDrop info: NSDraggingInfo, proposedItem item: AnyObject?, proposedChildIndex index: Int) -> NSDragOperation
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, acceptDrop info: NSDraggingInfo, item item: AnyObject?, childIndex index: Int) -> Bool
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, namesOfPromisedFilesDroppedAtDestination dropDestination: NSURL, forDraggedItems items: [AnyObject]) -> [String]
}
protocol NSOutlineViewDelegate : NSControlTextEditingDelegate {
  @available(OSX 10.7, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, viewFor tableColumn: NSTableColumn?, item item: AnyObject) -> NSView?
  @available(OSX 10.7, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, rowViewForItem item: AnyObject) -> NSTableRowView?
  @available(OSX 10.7, *)
  optional func outlineView(_ outlineView: NSOutlineView, didAdd rowView: NSTableRowView, forRow row: Int)
  @available(OSX 10.7, *)
  optional func outlineView(_ outlineView: NSOutlineView, didRemove rowView: NSTableRowView, forRow row: Int)
  optional func outlineView(_ outlineView: NSOutlineView, willDisplayCell cell: AnyObject, for tableColumn: NSTableColumn?, item item: AnyObject)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldEdit tableColumn: NSTableColumn?, item item: AnyObject) -> Bool
  @discardableResult
  optional func selectionShouldChange(in outlineView: NSOutlineView) -> Bool
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldSelectItem item: AnyObject) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, selectionIndexesForProposedSelection proposedSelectionIndexes: NSIndexSet) -> NSIndexSet
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldSelect tableColumn: NSTableColumn?) -> Bool
  optional func outlineView(_ outlineView: NSOutlineView, mouseDownInHeaderOf tableColumn: NSTableColumn)
  optional func outlineView(_ outlineView: NSOutlineView, didClick tableColumn: NSTableColumn)
  optional func outlineView(_ outlineView: NSOutlineView, didDrag tableColumn: NSTableColumn)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, toolTipFor cell: NSCell, rect rect: NSRectPointer, tableColumn tableColumn: NSTableColumn?, item item: AnyObject, mouseLocation mouseLocation: NSPoint) -> String
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, heightOfRowByItem item: AnyObject) -> CGFloat
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, typeSelectStringFor tableColumn: NSTableColumn?, item item: AnyObject) -> String?
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, nextTypeSelectMatchFromItem startItem: AnyObject, toItem endItem: AnyObject, for searchString: String) -> AnyObject?
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldTypeSelectFor event: NSEvent, withCurrentSearch searchString: String?) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldShowCellExpansionFor tableColumn: NSTableColumn?, item item: AnyObject) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldTrackCell cell: NSCell, for tableColumn: NSTableColumn?, item item: AnyObject) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, dataCellFor tableColumn: NSTableColumn?, item item: AnyObject) -> NSCell?
  @available(OSX 10.5, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, isGroupItem item: AnyObject) -> Bool
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldExpandItem item: AnyObject) -> Bool
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldCollapseItem item: AnyObject) -> Bool
  optional func outlineView(_ outlineView: NSOutlineView, willDisplayOutlineCell cell: AnyObject, for tableColumn: NSTableColumn?, item item: AnyObject)
  @available(OSX 10.6, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, sizeToFitWidthOfColumn column: Int) -> CGFloat
  @available(OSX 10.6, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldReorderColumn columnIndex: Int, toColumn newColumnIndex: Int) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  optional func outlineView(_ outlineView: NSOutlineView, shouldShowOutlineCellForItem item: AnyObject) -> Bool
  optional func outlineViewSelectionDidChange(_ notification: NSNotification)
  optional func outlineViewColumnDidMove(_ notification: NSNotification)
  optional func outlineViewColumnDidResize(_ notification: NSNotification)
  optional func outlineViewSelectionIsChanging(_ notification: NSNotification)
  optional func outlineViewItemWillExpand(_ notification: NSNotification)
  optional func outlineViewItemDidExpand(_ notification: NSNotification)
  optional func outlineViewItemWillCollapse(_ notification: NSNotification)
  optional func outlineViewItemDidCollapse(_ notification: NSNotification)
}
@available(OSX 10.9, *)
let NSOutlineViewDisclosureButtonKey: String
@available(OSX 10.9, *)
let NSOutlineViewShowHideButtonKey: String
let NSOutlineViewSelectionDidChangeNotification: String
let NSOutlineViewColumnDidMoveNotification: String
let NSOutlineViewColumnDidResizeNotification: String
let NSOutlineViewSelectionIsChangingNotification: String
let NSOutlineViewItemWillExpandNotification: String
let NSOutlineViewItemDidExpandNotification: String
let NSOutlineViewItemWillCollapseNotification: String
let NSOutlineViewItemDidCollapseNotification: String
