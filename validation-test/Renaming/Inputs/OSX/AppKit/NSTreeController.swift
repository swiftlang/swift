
class NSTreeController : NSObjectController {
  func rearrangeObjects()
  var arrangedObjects: AnyObject { get }
  var childrenKeyPath: String?
  var countKeyPath: String?
  var leafKeyPath: String?
  var sortDescriptors: [NSSortDescriptor]
  func addChild(_ sender: AnyObject?)
  func insert(_ sender: AnyObject?)
  func insertChild(_ sender: AnyObject?)
  var canInsert: Bool { get }
  var canInsertChild: Bool { get }
  var canAddChild: Bool { get }
  func insert(_ object: AnyObject?, atArrangedObjectIndexPath indexPath: NSIndexPath)
  func insert(_ objects: [AnyObject], atArrangedObjectIndexPaths indexPaths: [NSIndexPath])
  func removeObject(atArrangedObjectIndexPath indexPath: NSIndexPath)
  func removeObjects(atArrangedObjectIndexPaths indexPaths: [NSIndexPath])
  var avoidsEmptySelection: Bool
  var preservesSelection: Bool
  var selectsInsertedObjects: Bool
  var alwaysUsesMultipleValuesMarker: Bool
  @discardableResult
  func setSelectionIndexPaths(_ indexPaths: [NSIndexPath]) -> Bool
  var selectionIndexPaths: [NSIndexPath] { get }
  @discardableResult
  func setSelectionIndexPath(_ indexPath: NSIndexPath?) -> Bool
  @NSCopying var selectionIndexPath: NSIndexPath? { get }
  @discardableResult
  func addSelectionIndexPaths(_ indexPaths: [NSIndexPath]) -> Bool
  @discardableResult
  func removeSelectionIndexPaths(_ indexPaths: [NSIndexPath]) -> Bool
  @available(OSX 10.5, *)
  var selectedNodes: [NSTreeNode] { get }
  @available(OSX 10.5, *)
  func move(_ node: NSTreeNode, to indexPath: NSIndexPath)
  @available(OSX 10.5, *)
  func move(_ nodes: [NSTreeNode], to startingIndexPath: NSIndexPath)
  @available(OSX 10.5, *)
  @discardableResult
  func childrenKeyPath(for node: NSTreeNode) -> String?
  @available(OSX 10.5, *)
  @discardableResult
  func countKeyPath(for node: NSTreeNode) -> String?
  @available(OSX 10.5, *)
  @discardableResult
  func leafKeyPath(for node: NSTreeNode) -> String?
}
struct __treeControllerFlags {
  var _avoidsEmptySelection: UInt32
  var _preservesSelection: UInt32
  var _selectsInsertedObjects: UInt32
  var _explicitlyCannotInsert: UInt32
  var _explicitlyCannotInsertChild: UInt32
  var _explicitlyCannotAddChild: UInt32
  var _alwaysUsesMultipleValuesMarker: UInt32
  var _observingThroughArrangedObjects: UInt32
  var _mutatingNodes: UInt32
  var _performingFetch: UInt32
  var _skipSortingAfterFetch: UInt32
  var _usesIdenticalComparisonOfModelObjects: UInt32
  var _reservedTreeController: UInt32
  init()
  init(_avoidsEmptySelection _avoidsEmptySelection: UInt32, _preservesSelection _preservesSelection: UInt32, _selectsInsertedObjects _selectsInsertedObjects: UInt32, _explicitlyCannotInsert _explicitlyCannotInsert: UInt32, _explicitlyCannotInsertChild _explicitlyCannotInsertChild: UInt32, _explicitlyCannotAddChild _explicitlyCannotAddChild: UInt32, _alwaysUsesMultipleValuesMarker _alwaysUsesMultipleValuesMarker: UInt32, _observingThroughArrangedObjects _observingThroughArrangedObjects: UInt32, _mutatingNodes _mutatingNodes: UInt32, _performingFetch _performingFetch: UInt32, _skipSortingAfterFetch _skipSortingAfterFetch: UInt32, _usesIdenticalComparisonOfModelObjects _usesIdenticalComparisonOfModelObjects: UInt32, _reservedTreeController _reservedTreeController: UInt32)
}
