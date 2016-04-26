
class NSArrayController : NSObjectController {
  func rearrangeObjects()
  @available(OSX 10.5, *)
  var automaticallyRearrangesObjects: Bool
  @available(OSX 10.5, *)
  var automaticRearrangementKeyPaths: [String]? { get }
  @available(OSX 10.5, *)
  func didChangeArrangementCriteria()
  var sortDescriptors: [NSSortDescriptor]
  var filterPredicate: NSPredicate?
  var clearsFilterPredicateOnInsertion: Bool
  @discardableResult
  func arrange(_ objects: [AnyObject]) -> [AnyObject]
  var arrangedObjects: AnyObject { get }
  var avoidsEmptySelection: Bool
  var preservesSelection: Bool
  var selectsInsertedObjects: Bool
  var alwaysUsesMultipleValuesMarker: Bool
  @discardableResult
  func setSelectionIndexes(_ indexes: NSIndexSet) -> Bool
  @NSCopying var selectionIndexes: NSIndexSet { get }
  @discardableResult
  func setSelectionIndex(_ index: Int) -> Bool
  var selectionIndex: Int { get }
  @discardableResult
  func addSelectionIndexes(_ indexes: NSIndexSet) -> Bool
  @discardableResult
  func removeSelectionIndexes(_ indexes: NSIndexSet) -> Bool
  @discardableResult
  func setSelectedObjects(_ objects: [AnyObject]) -> Bool
  @discardableResult
  func addSelectedObjects(_ objects: [AnyObject]) -> Bool
  @discardableResult
  func removeSelectedObjects(_ objects: [AnyObject]) -> Bool
  func insert(_ sender: AnyObject?)
  var canInsert: Bool { get }
  func selectNext(_ sender: AnyObject?)
  func selectPrevious(_ sender: AnyObject?)
  var canSelectNext: Bool { get }
  var canSelectPrevious: Bool { get }
  func add(_ objects: [AnyObject])
  func insert(_ object: AnyObject, atArrangedObjectIndex index: Int)
  func insert(_ objects: [AnyObject], atArrangedObjectIndexes indexes: NSIndexSet)
  func removeObject(atArrangedObjectIndex index: Int)
  func removeObjects(atArrangedObjectIndexes indexes: NSIndexSet)
  func remove(_ objects: [AnyObject])
}
struct __arrayControllerFlags {
  var _avoidsEmptySelection: UInt32
  var _preservesSelection: UInt32
  var _selectsInsertedObjects: UInt32
  var _alwaysUsesMultipleValuesMarker: UInt32
  var _refreshesAllModelObjects: UInt32
  var _filterRestrictsInsertion: UInt32
  var _overridesArrangeObjects: UInt32
  var _overridesDidChangeArrangementCriteria: UInt32
  var _explicitlyCannotInsert: UInt32
  var _generatedEmptyArray: UInt32
  var _isObservingKeyPathsThroughArrangedObjects: UInt32
  var _arrangedObjectsIsMutable: UInt32
  var _clearsFilterPredicateOnInsertion: UInt32
  var _skipSortingAfterFetch: UInt32
  var _automaticallyRearrangesObjects: UInt32
  var _reservedArrayController: UInt32
  init()
  init(_avoidsEmptySelection _avoidsEmptySelection: UInt32, _preservesSelection _preservesSelection: UInt32, _selectsInsertedObjects _selectsInsertedObjects: UInt32, _alwaysUsesMultipleValuesMarker _alwaysUsesMultipleValuesMarker: UInt32, _refreshesAllModelObjects _refreshesAllModelObjects: UInt32, _filterRestrictsInsertion _filterRestrictsInsertion: UInt32, _overridesArrangeObjects _overridesArrangeObjects: UInt32, _overridesDidChangeArrangementCriteria _overridesDidChangeArrangementCriteria: UInt32, _explicitlyCannotInsert _explicitlyCannotInsert: UInt32, _generatedEmptyArray _generatedEmptyArray: UInt32, _isObservingKeyPathsThroughArrangedObjects _isObservingKeyPathsThroughArrangedObjects: UInt32, _arrangedObjectsIsMutable _arrangedObjectsIsMutable: UInt32, _clearsFilterPredicateOnInsertion _clearsFilterPredicateOnInsertion: UInt32, _skipSortingAfterFetch _skipSortingAfterFetch: UInt32, _automaticallyRearrangesObjects _automaticallyRearrangesObjects: UInt32, _reservedArrayController _reservedArrayController: UInt32)
}
