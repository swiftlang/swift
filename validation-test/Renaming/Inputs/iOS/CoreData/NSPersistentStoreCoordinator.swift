
@available(iOS 3.0, *)
let NSSQLiteStoreType: String
@available(iOS 3.0, *)
let NSBinaryStoreType: String
@available(iOS 3.0, *)
let NSInMemoryStoreType: String
@available(iOS 7.0, *)
enum NSPersistentStoreUbiquitousTransitionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case accountAdded
  case accountRemoved
  case contentRemoved
  case initialImportCompleted
}
@available(iOS 3.0, *)
let NSStoreTypeKey: String
@available(iOS 3.0, *)
let NSStoreUUIDKey: String
@available(iOS 7.0, *)
let NSPersistentStoreCoordinatorStoresWillChangeNotification: String
@available(iOS 3.0, *)
let NSPersistentStoreCoordinatorStoresDidChangeNotification: String
@available(iOS 3.0, *)
let NSPersistentStoreCoordinatorWillRemoveStoreNotification: String
@available(iOS 3.0, *)
let NSAddedPersistentStoresKey: String
@available(iOS 3.0, *)
let NSRemovedPersistentStoresKey: String
@available(iOS 3.0, *)
let NSUUIDChangedPersistentStoresKey: String
@available(iOS 3.0, *)
let NSReadOnlyPersistentStoreOption: String
@available(iOS 3.0, *)
let NSPersistentStoreTimeoutOption: String
@available(iOS 3.0, *)
let NSSQLitePragmasOption: String
@available(iOS 3.0, *)
let NSSQLiteAnalyzeOption: String
@available(iOS 3.0, *)
let NSSQLiteManualVacuumOption: String
@available(iOS 3.0, *)
let NSIgnorePersistentStoreVersioningOption: String
@available(iOS 3.0, *)
let NSMigratePersistentStoresAutomaticallyOption: String
@available(iOS 3.0, *)
let NSInferMappingModelAutomaticallyOption: String
@available(iOS 3.0, *)
let NSStoreModelVersionHashesKey: String
@available(iOS 3.0, *)
let NSStoreModelVersionIdentifiersKey: String
@available(iOS 3.0, *)
let NSPersistentStoreOSCompatibility: String
@available(iOS 5.0, *)
let NSPersistentStoreUbiquitousContentNameKey: String
@available(iOS 5.0, *)
let NSPersistentStoreUbiquitousContentURLKey: String
@available(iOS 5.0, *)
let NSPersistentStoreDidImportUbiquitousContentChangesNotification: String
@available(iOS 7.0, *)
let NSPersistentStoreUbiquitousTransitionTypeKey: String
@available(iOS 7.0, *)
let NSPersistentStoreUbiquitousPeerTokenOption: String
@available(iOS 7.0, *)
let NSPersistentStoreRemoveUbiquitousMetadataOption: String
@available(iOS 7.0, *)
let NSPersistentStoreUbiquitousContainerIdentifierKey: String
@available(iOS 7.0, *)
let NSPersistentStoreRebuildFromUbiquitousContentOption: String
@available(iOS 6.0, *)
let NSPersistentStoreForceDestroyOption: String
@available(iOS 5.0, *)
let NSPersistentStoreFileProtectionKey: String
@available(iOS 3.0, *)
class NSPersistentStoreCoordinator : NSObject, NSLocking {
  init(managedObjectModel model: NSManagedObjectModel)
  var managedObjectModel: NSManagedObjectModel { get }
  var persistentStores: [NSPersistentStore] { get }
  @available(iOS 8.0, *)
  var name: String?
  @discardableResult
  func persistentStore(for URL: NSURL) -> NSPersistentStore?
  @discardableResult
  func url(for store: NSPersistentStore) -> NSURL
  @available(iOS 3.0, *)
  @discardableResult
  func setURL(_ url: NSURL, for store: NSPersistentStore) -> Bool
  @discardableResult
  func addPersistentStore(ofType storeType: String, configurationName configuration: String?, at storeURL: NSURL?, options options: [NSObject : AnyObject]? = [:]) throws -> NSPersistentStore
  func remove(_ store: NSPersistentStore) throws
  func setMetadata(_ metadata: [String : AnyObject]?, for store: NSPersistentStore)
  @discardableResult
  func metadata(for store: NSPersistentStore) -> [String : AnyObject]
  @discardableResult
  func managedObjectID(forURIRepresentation url: NSURL) -> NSManagedObjectID?
  @available(iOS 5.0, *)
  @discardableResult
  func execute(_ request: NSPersistentStoreRequest, with context: NSManagedObjectContext) throws -> AnyObject
  @available(iOS 3.0, *)
  @discardableResult
  class func registeredStoreTypes() -> [String : NSValue]
  @available(iOS 3.0, *)
  class func registerStoreClass(_ storeClass: AnyClass, forStoreType storeType: String)
  @available(iOS 7.0, *)
  @discardableResult
  class func metadataForPersistentStore(ofType storeType: String, at url: NSURL, options options: [NSObject : AnyObject]? = [:]) throws -> [String : AnyObject]
  @available(iOS 7.0, *)
  class func setMetadata(_ metadata: [String : AnyObject]?, forPersistentStoreOfType storeType: String, at url: NSURL, options options: [NSObject : AnyObject]? = [:]) throws
  @available(iOS, introduced: 3.0, deprecated: 9.0, message: "Use a -metadataForPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType")
  @discardableResult
  class func metadataForPersistentStore(ofType storeType: String?, url url: NSURL) throws -> [String : AnyObject]
  @available(iOS, introduced: 3.0, deprecated: 9.0, message: "Use a -setMetadata:forPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType")
  class func setMetadata(_ metadata: [String : AnyObject]?, forPersistentStoreOfType storeType: String?, url url: NSURL) throws
  @available(iOS 7.0, *)
  class func removeUbiquitousContentAndPersistentStore(at storeURL: NSURL, options options: [NSObject : AnyObject]? = [:]) throws
  @discardableResult
  func migratePersistentStore(_ store: NSPersistentStore, to URL: NSURL, options options: [NSObject : AnyObject]? = [:], withType storeType: String) throws -> NSPersistentStore
  @available(iOS 9.0, *)
  func destroyPersistentStore(at url: NSURL, withType storeType: String, options options: [NSObject : AnyObject]? = [:]) throws
  @available(iOS 9.0, *)
  func replacePersistentStore(at destinationURL: NSURL, destinationOptions destinationOptions: [NSObject : AnyObject]? = [:], withPersistentStoreFrom sourceURL: NSURL, sourceOptions sourceOptions: [NSObject : AnyObject]? = [:], storeType storeType: String) throws
  @available(iOS 8.0, *)
  func perform(_ block: () -> Void)
  @available(iOS 8.0, *)
  func performBlockAndWait(_ block: () -> Void)
  @available(iOS, introduced: 3.0, deprecated: 8.0, message: "Use -performBlock: instead")
  @discardableResult
  func tryLock() -> Bool
}
struct _persistentStoreCoordinatorFlags {
  var _isRegistered: UInt32
  var _reservedFlags: UInt32
  init()
  init(_isRegistered _isRegistered: UInt32, _reservedFlags _reservedFlags: UInt32)
}
