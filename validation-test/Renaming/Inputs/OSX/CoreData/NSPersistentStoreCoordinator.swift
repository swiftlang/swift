
@available(OSX 10.4, *)
let NSSQLiteStoreType: String
@available(OSX 10.4, *)
let NSXMLStoreType: String
@available(OSX 10.4, *)
let NSBinaryStoreType: String
@available(OSX 10.4, *)
let NSInMemoryStoreType: String
@available(OSX 10.9, *)
enum NSPersistentStoreUbiquitousTransitionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case accountAdded
  case accountRemoved
  case contentRemoved
  case initialImportCompleted
}
@available(OSX 10.4, *)
let NSStoreTypeKey: String
@available(OSX 10.4, *)
let NSStoreUUIDKey: String
@available(OSX 10.9, *)
let NSPersistentStoreCoordinatorStoresWillChangeNotification: String
@available(OSX 10.4, *)
let NSPersistentStoreCoordinatorStoresDidChangeNotification: String
@available(OSX 10.5, *)
let NSPersistentStoreCoordinatorWillRemoveStoreNotification: String
@available(OSX 10.4, *)
let NSAddedPersistentStoresKey: String
@available(OSX 10.4, *)
let NSRemovedPersistentStoresKey: String
@available(OSX 10.4, *)
let NSUUIDChangedPersistentStoresKey: String
@available(OSX 10.4, *)
let NSReadOnlyPersistentStoreOption: String
@available(OSX 10.4, *)
let NSValidateXMLStoreOption: String
@available(OSX 10.5, *)
let NSPersistentStoreTimeoutOption: String
@available(OSX 10.5, *)
let NSSQLitePragmasOption: String
@available(OSX 10.5, *)
let NSSQLiteAnalyzeOption: String
@available(OSX 10.6, *)
let NSSQLiteManualVacuumOption: String
@available(OSX 10.5, *)
let NSIgnorePersistentStoreVersioningOption: String
@available(OSX 10.5, *)
let NSMigratePersistentStoresAutomaticallyOption: String
@available(OSX 10.6, *)
let NSInferMappingModelAutomaticallyOption: String
@available(OSX 10.5, *)
let NSStoreModelVersionHashesKey: String
@available(OSX 10.5, *)
let NSStoreModelVersionIdentifiersKey: String
@available(OSX 10.5, *)
let NSPersistentStoreOSCompatibility: String
@available(OSX 10.6, *)
let NSXMLExternalRecordType: String
@available(OSX 10.6, *)
let NSBinaryExternalRecordType: String
@available(OSX 10.6, *)
let NSExternalRecordsFileFormatOption: String
@available(OSX 10.6, *)
let NSExternalRecordsDirectoryOption: String
@available(OSX 10.6, *)
let NSExternalRecordExtensionOption: String
@available(OSX 10.6, *)
let NSEntityNameInPathKey: String
@available(OSX 10.6, *)
let NSStoreUUIDInPathKey: String
@available(OSX 10.6, *)
let NSStorePathKey: String
@available(OSX 10.6, *)
let NSModelPathKey: String
@available(OSX 10.6, *)
let NSObjectURIKey: String
@available(OSX 10.7, *)
let NSPersistentStoreUbiquitousContentNameKey: String
@available(OSX 10.7, *)
let NSPersistentStoreUbiquitousContentURLKey: String
@available(OSX 10.7, *)
let NSPersistentStoreDidImportUbiquitousContentChangesNotification: String
@available(OSX 10.9, *)
let NSPersistentStoreUbiquitousTransitionTypeKey: String
@available(OSX 10.9, *)
let NSPersistentStoreUbiquitousPeerTokenOption: String
@available(OSX 10.9, *)
let NSPersistentStoreRemoveUbiquitousMetadataOption: String
@available(OSX 10.9, *)
let NSPersistentStoreUbiquitousContainerIdentifierKey: String
@available(OSX 10.9, *)
let NSPersistentStoreRebuildFromUbiquitousContentOption: String
@available(OSX 10.8, *)
let NSPersistentStoreForceDestroyOption: String
@available(OSX 10.4, *)
class NSPersistentStoreCoordinator : NSObject, NSLocking {
  init(managedObjectModel model: NSManagedObjectModel)
  var managedObjectModel: NSManagedObjectModel { get }
  var persistentStores: [NSPersistentStore] { get }
  @available(OSX 10.10, *)
  var name: String?
  @discardableResult
  func persistentStore(for URL: NSURL) -> NSPersistentStore?
  @discardableResult
  func url(for store: NSPersistentStore) -> NSURL
  @available(OSX 10.5, *)
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
  @available(OSX 10.7, *)
  @discardableResult
  func execute(_ request: NSPersistentStoreRequest, with context: NSManagedObjectContext) throws -> AnyObject
  @available(OSX 10.5, *)
  @discardableResult
  class func registeredStoreTypes() -> [String : NSValue]
  @available(OSX 10.5, *)
  class func registerStoreClass(_ storeClass: AnyClass, forStoreType storeType: String)
  @available(OSX 10.9, *)
  @discardableResult
  class func metadataForPersistentStore(ofType storeType: String, at url: NSURL, options options: [NSObject : AnyObject]? = [:]) throws -> [String : AnyObject]
  @available(OSX 10.9, *)
  class func setMetadata(_ metadata: [String : AnyObject]?, forPersistentStoreOfType storeType: String, at url: NSURL, options options: [NSObject : AnyObject]? = [:]) throws
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use a -metadataForPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType")
  @discardableResult
  class func metadataForPersistentStore(ofType storeType: String?, url url: NSURL) throws -> [String : AnyObject]
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use a -setMetadata:forPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType")
  class func setMetadata(_ metadata: [String : AnyObject]?, forPersistentStoreOfType storeType: String?, url url: NSURL) throws
  @available(OSX 10.6, *)
  @discardableResult
  class func elementsDerived(fromExternalRecordAt fileURL: NSURL) -> [NSObject : AnyObject]
  @available(OSX 10.9, *)
  class func removeUbiquitousContentAndPersistentStore(at storeURL: NSURL, options options: [NSObject : AnyObject]? = [:]) throws
  @available(OSX 10.6, *)
  @discardableResult
  func importStore(withIdentifier storeIdentifier: String?, fromExternalRecordsDirectoryAt externalRecordsURL: NSURL, to destinationURL: NSURL, options options: [NSObject : AnyObject]? = [:], ofType storeType: String) throws -> NSPersistentStore
  @discardableResult
  func migratePersistentStore(_ store: NSPersistentStore, to URL: NSURL, options options: [NSObject : AnyObject]? = [:], withType storeType: String) throws -> NSPersistentStore
  @available(OSX 10.11, *)
  func destroyPersistentStore(at url: NSURL, withType storeType: String, options options: [NSObject : AnyObject]? = [:]) throws
  @available(OSX 10.11, *)
  func replacePersistentStore(at destinationURL: NSURL, destinationOptions destinationOptions: [NSObject : AnyObject]? = [:], withPersistentStoreFrom sourceURL: NSURL, sourceOptions sourceOptions: [NSObject : AnyObject]? = [:], storeType storeType: String) throws
  @available(OSX 10.10, *)
  func perform(_ block: () -> Void)
  @available(OSX 10.10, *)
  func performBlockAndWait(_ block: () -> Void)
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use -performBlock: instead")
  @discardableResult
  func tryLock() -> Bool
}
struct _persistentStoreCoordinatorFlags {
  var _isRegistered: UInt32
  var _reservedFlags: UInt32
  init()
  init(_isRegistered _isRegistered: UInt32, _reservedFlags _reservedFlags: UInt32)
}
