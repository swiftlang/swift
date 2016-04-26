
@available(OSX 10.6, *)
enum NSCollectionViewDropOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case on
  case before
}
@available(OSX 10.11, *)
enum NSCollectionViewItemHighlightState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case forSelection
  case forDeselection
  case asDropTarget
}
struct NSCollectionViewScrollPosition : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var top: NSCollectionViewScrollPosition { get }
  static var centeredVertically: NSCollectionViewScrollPosition { get }
  static var bottom: NSCollectionViewScrollPosition { get }
  static var nearestHorizontalEdge: NSCollectionViewScrollPosition { get }
  static var left: NSCollectionViewScrollPosition { get }
  static var centeredHorizontally: NSCollectionViewScrollPosition { get }
  static var right: NSCollectionViewScrollPosition { get }
  static var leadingEdge: NSCollectionViewScrollPosition { get }
  static var trailingEdge: NSCollectionViewScrollPosition { get }
  static var nearestVerticalEdge: NSCollectionViewScrollPosition { get }
}
protocol NSCollectionViewElement : NSObjectProtocol, NSUserInterfaceItemIdentification {
  @available(OSX 10.11, *)
  optional func prepareForReuse()
  @available(OSX 10.11, *)
  optional func apply(_ layoutAttributes: NSCollectionViewLayoutAttributes)
  @available(OSX 10.11, *)
  optional func willTransition(from oldLayout: NSCollectionViewLayout, to newLayout: NSCollectionViewLayout)
  @available(OSX 10.11, *)
  optional func didTransition(from oldLayout: NSCollectionViewLayout, to newLayout: NSCollectionViewLayout)
  @available(OSX 10.11, *)
  @discardableResult
  optional func preferredLayoutAttributesFitting(_ layoutAttributes: NSCollectionViewLayoutAttributes) -> NSCollectionViewLayoutAttributes
}
@available(OSX 10.5, *)
class NSCollectionViewItem : NSViewController, NSCopying, NSCollectionViewElement {
  var collectionView: NSCollectionView { get }
  var isSelected: Bool
  @available(OSX 10.11, *)
  var highlightState: NSCollectionViewItemHighlightState
  @available(OSX 10.7, *)
  @IBOutlet unowned(unsafe) var imageView: @sil_unmanaged NSImageView?
  @available(OSX 10.7, *)
  @IBOutlet unowned(unsafe) var textField: @sil_unmanaged NSTextField?
  @available(OSX 10.7, *)
  var draggingImageComponents: [NSDraggingImageComponent] { get }
}
@available(OSX 10.5, *)
class NSCollectionView : NSView, NSDraggingSource, NSDraggingDestination {
  @available(OSX 10.11, *)
  weak var dataSource: @sil_weak NSCollectionViewDataSource?
  var content: [AnyObject]
  @available(OSX 10.11, *)
  func reloadData()
  unowned(unsafe) var delegate: @sil_unmanaged NSCollectionViewDelegate?
  @available(OSX 10.11, *)
  var backgroundView: NSView?
  @available(OSX 10.11, *)
  var collectionViewLayout: NSCollectionViewLayout?
  @available(OSX 10.11, *)
  @discardableResult
  func layoutAttributesForItem(at indexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @available(OSX 10.11, *)
  @discardableResult
  func layoutAttributesForSupplementaryElement(ofKind kind: String, at indexPath: NSIndexPath) -> NSCollectionViewLayoutAttributes?
  @available(OSX 10.6, *)
  @discardableResult
  func frameForItem(at index: Int) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func frameForItem(at index: Int, withNumberOfItems numberOfItems: Int) -> NSRect
  var maxNumberOfRows: Int
  var maxNumberOfColumns: Int
  var minItemSize: NSSize
  var maxItemSize: NSSize
  var backgroundColors: [NSColor]!
  @available(OSX 10.11, *)
  var numberOfSections: Int { get }
  @available(OSX 10.11, *)
  @discardableResult
  func numberOfItems(inSection section: Int) -> Int
  var isFirstResponder: Bool { get }
  var isSelectable: Bool
  @available(OSX 10.11, *)
  var allowsEmptySelection: Bool
  var allowsMultipleSelection: Bool
  @NSCopying var selectionIndexes: NSIndexSet
  @available(OSX 10.11, *)
  var selectionIndexPaths: Set<NSIndexPath>
  @available(OSX 10.11, *)
  func selectItems(at indexPaths: Set<NSIndexPath>, scrollPosition scrollPosition: NSCollectionViewScrollPosition)
  @available(OSX 10.11, *)
  func deselectItems(at indexPaths: Set<NSIndexPath>)
  @IBAction func deselectAll(_ sender: AnyObject?)
  @available(OSX 10.11, *)
  func register(_ itemClass: AnyClass?, forItemWithIdentifier identifier: String)
  @available(OSX 10.11, *)
  func register(_ nib: NSNib?, forItemWithIdentifier identifier: String)
  @available(OSX 10.11, *)
  func register(_ viewClass: AnyClass?, forSupplementaryViewOfKind kind: String, withIdentifier identifier: String)
  @available(OSX 10.11, *)
  func register(_ nib: NSNib?, forSupplementaryViewOfKind kind: String, withIdentifier identifier: String)
  @available(OSX 10.11, *)
  @discardableResult
  func makeItem(withIdentifier identifier: String, for indexPath: NSIndexPath) -> NSCollectionViewItem
  @available(OSX 10.11, *)
  @discardableResult
  func makeSupplementaryView(ofKind elementKind: String, withIdentifier identifier: String, for indexPath: NSIndexPath) -> NSView
  @discardableResult
  func newItem(forRepresentedObject object: AnyObject) -> NSCollectionViewItem
  var itemPrototype: NSCollectionViewItem?
  @available(OSX 10.6, *)
  @discardableResult
  func item(at index: Int) -> NSCollectionViewItem?
  @available(OSX 10.11, *)
  @discardableResult
  func item(at indexPath: NSIndexPath) -> NSCollectionViewItem?
  @available(OSX 10.11, *)
  @discardableResult
  func visibleItems() -> [NSCollectionViewItem]
  @available(OSX 10.11, *)
  @discardableResult
  func indexPathsForVisibleItems() -> Set<NSIndexPath>
  @available(OSX 10.11, *)
  @discardableResult
  func indexPath(for item: NSCollectionViewItem) -> NSIndexPath?
  @available(OSX 10.11, *)
  @discardableResult
  func indexPathForItem(at point: NSPoint) -> NSIndexPath?
  @available(OSX 10.11, *)
  @discardableResult
  func supplementaryView(forElementKind elementKind: String, at indexPath: NSIndexPath) -> NSView?
  @available(OSX 10.11, *)
  @discardableResult
  func visibleSupplementaryViews(ofKind elementKind: String) -> [NSView]
  @available(OSX 10.11, *)
  @discardableResult
  func indexPathsForVisibleSupplementaryElements(ofKind elementKind: String) -> Set<NSIndexPath>
  @available(OSX 10.11, *)
  func insertSections(_ sections: NSIndexSet)
  @available(OSX 10.11, *)
  func deleteSections(_ sections: NSIndexSet)
  @available(OSX 10.11, *)
  func reloadSections(_ sections: NSIndexSet)
  @available(OSX 10.11, *)
  func moveSection(_ section: Int, toSection newSection: Int)
  @available(OSX 10.11, *)
  func insertItems(at indexPaths: Set<NSIndexPath>)
  @available(OSX 10.11, *)
  func deleteItems(at indexPaths: Set<NSIndexPath>)
  @available(OSX 10.11, *)
  func reloadItems(at indexPaths: Set<NSIndexPath>)
  @available(OSX 10.11, *)
  func moveItem(at indexPath: NSIndexPath, to newIndexPath: NSIndexPath)
  @available(OSX 10.11, *)
  func performBatchUpdates(_ updates: (() -> Void)?, completionHandler completionHandler: ((Bool) -> Void)? = nil)
  @available(OSX 10.11, *)
  func scrollToItems(at indexPaths: Set<NSIndexPath>, scrollPosition scrollPosition: NSCollectionViewScrollPosition)
  @available(OSX 10.6, *)
  func setDraggingSourceOperationMask(_ dragOperationMask: NSDragOperation, forLocal localDestination: Bool)
  @available(OSX 10.11, *)
  @discardableResult
  func draggingImageForItems(at indexPaths: Set<NSIndexPath>, with event: NSEvent, offset dragImageOffset: NSPointPointer) -> NSImage
  @available(OSX 10.6, *)
  @discardableResult
  func draggingImageForItems(at indexes: NSIndexSet, with event: NSEvent, offset dragImageOffset: NSPointPointer) -> NSImage
}
protocol NSCollectionViewDataSource : NSObjectProtocol {
  @available(OSX 10.11, *)
  @discardableResult
  func collectionView(_ collectionView: NSCollectionView, numberOfItemsInSection section: Int) -> Int
  @available(OSX 10.11, *)
  @discardableResult
  func collectionView(_ collectionView: NSCollectionView, itemForRepresentedObjectAt indexPath: NSIndexPath) -> NSCollectionViewItem
  @available(OSX 10.11, *)
  @discardableResult
  optional func numberOfSections(in collectionView: NSCollectionView) -> Int
  @available(OSX 10.5, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, viewForSupplementaryElementOfKind kind: String, at indexPath: NSIndexPath) -> NSView
}
protocol NSCollectionViewDelegate : NSObjectProtocol {
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, canDragItemsAt indexPaths: Set<NSIndexPath>, with event: NSEvent) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, canDragItemsAt indexes: NSIndexSet, with event: NSEvent) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, writeItemsAt indexPaths: Set<NSIndexPath>, to pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, writeItemsAt indexes: NSIndexSet, to pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, namesOfPromisedFilesDroppedAtDestination dropURL: NSURL, forDraggedItemsAt indexPaths: Set<NSIndexPath>) -> [String]
  @available(OSX 10.6, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, namesOfPromisedFilesDroppedAtDestination dropURL: NSURL, forDraggedItemsAt indexes: NSIndexSet) -> [String]
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, draggingImageForItemsAt indexPaths: Set<NSIndexPath>, with event: NSEvent, offset dragImageOffset: NSPointPointer) -> NSImage
  @available(OSX 10.6, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, draggingImageForItemsAt indexes: NSIndexSet, with event: NSEvent, offset dragImageOffset: NSPointPointer) -> NSImage
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, validateDrop draggingInfo: NSDraggingInfo, proposedIndexPath proposedDropIndexPath: AutoreleasingUnsafeMutablePointer<NSIndexPath>, dropOperation proposedDropOperation: UnsafeMutablePointer<NSCollectionViewDropOperation>) -> NSDragOperation
  @available(OSX 10.6, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, validateDrop draggingInfo: NSDraggingInfo, proposedIndex proposedDropIndex: UnsafeMutablePointer<Int>, dropOperation proposedDropOperation: UnsafeMutablePointer<NSCollectionViewDropOperation>) -> NSDragOperation
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, acceptDrop draggingInfo: NSDraggingInfo, indexPath indexPath: NSIndexPath, dropOperation dropOperation: NSCollectionViewDropOperation) -> Bool
  @available(OSX 10.6, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, acceptDrop draggingInfo: NSDraggingInfo, index index: Int, dropOperation dropOperation: NSCollectionViewDropOperation) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, pasteboardWriterForItemAt indexPath: NSIndexPath) -> NSPasteboardWriting?
  @available(OSX 10.5, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, pasteboardWriterForItemAt index: Int) -> NSPasteboardWriting?
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, draggingSession session: NSDraggingSession, willBeginAt screenPoint: NSPoint, forItemsAt indexPaths: Set<NSIndexPath>)
  @available(OSX 10.7, *)
  optional func collectionView(_ collectionView: NSCollectionView, draggingSession session: NSDraggingSession, willBeginAt screenPoint: NSPoint, forItemsAt indexes: NSIndexSet)
  @available(OSX 10.7, *)
  optional func collectionView(_ collectionView: NSCollectionView, draggingSession session: NSDraggingSession, endedAt screenPoint: NSPoint, dragOperation operation: NSDragOperation)
  @available(OSX 10.5, *)
  optional func collectionView(_ collectionView: NSCollectionView, updateDraggingItemsForDrag draggingInfo: NSDraggingInfo)
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, shouldChangeItemsAt indexPaths: Set<NSIndexPath>, to highlightState: NSCollectionViewItemHighlightState) -> Set<NSIndexPath>
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, didChangeItemsAt indexPaths: Set<NSIndexPath>, to highlightState: NSCollectionViewItemHighlightState)
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, shouldSelectItemsAt indexPaths: Set<NSIndexPath>) -> Set<NSIndexPath>
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, shouldDeselectItemsAt indexPaths: Set<NSIndexPath>) -> Set<NSIndexPath>
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, didSelectItemsAt indexPaths: Set<NSIndexPath>)
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, didDeselectItemsAt indexPaths: Set<NSIndexPath>)
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, willDisplay item: NSCollectionViewItem, forRepresentedObjectAt indexPath: NSIndexPath)
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, willDisplaySupplementaryView view: NSView, forElementKind elementKind: String, at indexPath: NSIndexPath)
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, didEndDisplaying item: NSCollectionViewItem, forRepresentedObjectAt indexPath: NSIndexPath)
  @available(OSX 10.11, *)
  optional func collectionView(_ collectionView: NSCollectionView, didEndDisplayingSupplementaryView view: NSView, forElementOfKind elementKind: String, at indexPath: NSIndexPath)
  @available(OSX 10.11, *)
  @discardableResult
  optional func collectionView(_ collectionView: NSCollectionView, transitionLayoutForOldLayout fromLayout: NSCollectionViewLayout, newLayout toLayout: NSCollectionViewLayout) -> NSCollectionViewTransitionLayout
}
extension NSIndexPath {
  @available(OSX 10.11, *)
  /*not inherited*/ init(forItem item: Int, inSection section: Int)
  @available(OSX 10.11, *)
  var item: Int { get }
  @available(OSX 10.11, *)
  var section: Int { get }
}
extension NSSet {
  @available(OSX 10.11, *)
  convenience init(collectionViewIndexPath indexPath: NSIndexPath)
  @available(OSX 10.11, *)
  convenience init(collectionViewIndexPaths indexPaths: [NSIndexPath])
  @available(OSX 10.11, *)
  func enumerateIndexPaths(_ opts: NSEnumerationOptions = [], using block: (NSIndexPath, UnsafeMutablePointer<ObjCBool>) -> Void)
}
