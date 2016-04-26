
@available(tvOS 3.0, *)
let NSManagedObjectContextWillSaveNotification: String
@available(tvOS 3.0, *)
let NSManagedObjectContextDidSaveNotification: String
@available(tvOS 3.0, *)
let NSManagedObjectContextObjectsDidChangeNotification: String
@available(tvOS 3.0, *)
let NSInsertedObjectsKey: String
@available(tvOS 3.0, *)
let NSUpdatedObjectsKey: String
@available(tvOS 3.0, *)
let NSDeletedObjectsKey: String
@available(tvOS 3.0, *)
let NSRefreshedObjectsKey: String
@available(tvOS 3.0, *)
let NSInvalidatedObjectsKey: String
@available(tvOS 3.0, *)
let NSInvalidatedAllObjectsKey: String
@available(tvOS 5.0, *)
enum NSManagedObjectContextConcurrencyType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  @available(tvOS, introduced: 3.0, deprecated: 9.0, message: "Use another NSManagedObjectContextConcurrencyType")
  case confinementConcurrencyType
  case privateQueueConcurrencyType
  case mainQueueConcurrencyType
}
@available(tvOS 3.0, *)
class NSManagedObjectContext : NSObject, NSCoding {
  @available(tvOS 5.0, *)
  init(concurrencyType ct: NSManagedObjectContextConcurrencyType)
  @available(tvOS 5.0, *)
  func perform(_ block: () -> Void)
  @available(tvOS 5.0, *)
  func performAndWait(_ block: () -> Void)
  var persistentStoreCoordinator: NSPersistentStoreCoordinator?
  @available(tvOS 5.0, *)
  var parent: NSManagedObjectContext?
  @available(tvOS 8.0, *)
  var name: String?
  var undoManager: NSUndoManager?
  var hasChanges: Bool { get }
  @available(tvOS 5.0, *)
  var userInfo: NSMutableDictionary { get }
  @available(tvOS 5.0, *)
  var concurrencyType: NSManagedObjectContextConcurrencyType { get }
  @discardableResult
  func registeredObject(for objectID: NSManagedObjectID) -> NSManagedObject?
  @discardableResult
  func object(with objectID: NSManagedObjectID) -> NSManagedObject
  @available(tvOS 3.0, *)
  @discardableResult
  func existingObject(with objectID: NSManagedObjectID) throws -> NSManagedObject
  @discardableResult
  func fetch(_ request: NSFetchRequest) throws -> [AnyObject]
  @available(tvOS 3.0, *)
  @discardableResult
  func count(for request: NSFetchRequest, error error: NSErrorPointer) -> Int
  @available(tvOS 8.0, *)
  @discardableResult
  func execute(_ request: NSPersistentStoreRequest) throws -> NSPersistentStoreResult
  func insert(_ object: NSManagedObject)
  func delete(_ object: NSManagedObject)
  func refresh(_ object: NSManagedObject, mergeChanges flag: Bool)
  func detectConflicts(for object: NSManagedObject)
  func processPendingChanges()
  func assign(_ object: AnyObject, to store: NSPersistentStore)
  var insertedObjects: Set<NSManagedObject> { get }
  var updatedObjects: Set<NSManagedObject> { get }
  var deletedObjects: Set<NSManagedObject> { get }
  var registeredObjects: Set<NSManagedObject> { get }
  func undo()
  func redo()
  func reset()
  func rollback()
  func save() throws
  @available(tvOS 8.3, *)
  func refreshAllObjects()
  var propagatesDeletesAtEndOfEvent: Bool
  var retainsRegisteredObjects: Bool
  @available(tvOS 9.0, *)
  var shouldDeleteInaccessibleFaults: Bool
  @available(tvOS 9.0, *)
  @discardableResult
  func shouldHandleInaccessibleFault(_ fault: NSManagedObject, for oid: NSManagedObjectID, triggeredByProperty property: NSPropertyDescription?) -> Bool
  var stalenessInterval: NSTimeInterval
  var mergePolicy: AnyObject
  @available(tvOS 3.0, *)
  func obtainPermanentIDs(for objects: [NSManagedObject]) throws
  @available(tvOS 3.0, *)
  func mergeChanges(fromContextDidSave notification: NSNotification)
  @available(tvOS 9.0, *)
  class func mergeChanges(fromRemoteContextSave changeNotificationData: [NSObject : AnyObject], into contexts: [NSManagedObjectContext])
}
struct _managedObjectContextFlags {
  var _registeredForCallback: UInt32
  var _propagatesDeletesAtEndOfEvent: UInt32
  var _exhaustiveValidation: UInt32
  var _processingChanges: UInt32
  var _useCommittedSnapshot: UInt32
  var _registeredUndoTransactionID: UInt32
  var _retainsAllRegisteredObjects: UInt32
  var _savingInProgress: UInt32
  var _wasDisposed: UInt32
  var _unprocessedChangesPending: UInt32
  var _isDirty: UInt32
  var _ignoreUndoCheckpoints: UInt32
  var _propagatingDeletes: UInt32
  var _isNSEditorEditing: UInt32
  var _isMainThreadBlessed: UInt32
  var _isImportContext: UInt32
  var _preflightSaveInProgress: UInt32
  var _disableDiscardEditing: UInt32
  var _isParentStoreContext: UInt32
  var _postSaveNotifications: UInt32
  var _isMerging: UInt32
  var _concurrencyType: UInt32
  var _deleteInaccessible: UInt32
  var _reservedFlags: UInt32
  init()
  init(_registeredForCallback _registeredForCallback: UInt32, _propagatesDeletesAtEndOfEvent _propagatesDeletesAtEndOfEvent: UInt32, _exhaustiveValidation _exhaustiveValidation: UInt32, _processingChanges _processingChanges: UInt32, _useCommittedSnapshot _useCommittedSnapshot: UInt32, _registeredUndoTransactionID _registeredUndoTransactionID: UInt32, _retainsAllRegisteredObjects _retainsAllRegisteredObjects: UInt32, _savingInProgress _savingInProgress: UInt32, _wasDisposed _wasDisposed: UInt32, _unprocessedChangesPending _unprocessedChangesPending: UInt32, _isDirty _isDirty: UInt32, _ignoreUndoCheckpoints _ignoreUndoCheckpoints: UInt32, _propagatingDeletes _propagatingDeletes: UInt32, _isNSEditorEditing _isNSEditorEditing: UInt32, _isMainThreadBlessed _isMainThreadBlessed: UInt32, _isImportContext _isImportContext: UInt32, _preflightSaveInProgress _preflightSaveInProgress: UInt32, _disableDiscardEditing _disableDiscardEditing: UInt32, _isParentStoreContext _isParentStoreContext: UInt32, _postSaveNotifications _postSaveNotifications: UInt32, _isMerging _isMerging: UInt32, _concurrencyType _concurrencyType: UInt32, _deleteInaccessible _deleteInaccessible: UInt32, _reservedFlags _reservedFlags: UInt32)
}
