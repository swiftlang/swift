
@available(tvOS 3.0, *)
class NSMigrationManager : NSObject {
  init(sourceModel sourceModel: NSManagedObjectModel, destinationModel destinationModel: NSManagedObjectModel)
  func migrateStore(from sourceURL: NSURL, sourceType sStoreType: String, options sOptions: [NSObject : AnyObject]? = [:], with mappings: NSMappingModel?, toDestinationURL dURL: NSURL, destinationType dStoreType: String, destinationOptions dOptions: [NSObject : AnyObject]? = [:]) throws
  @available(tvOS 5.0, *)
  var usesStoreSpecificMigrationManager: Bool
  func reset()
  var mappingModel: NSMappingModel { get }
  var sourceModel: NSManagedObjectModel { get }
  var destinationModel: NSManagedObjectModel { get }
  var sourceContext: NSManagedObjectContext { get }
  var destinationContext: NSManagedObjectContext { get }
  @discardableResult
  func sourceEntity(for mEntity: NSEntityMapping) -> NSEntityDescription?
  @discardableResult
  func destinationEntity(for mEntity: NSEntityMapping) -> NSEntityDescription?
  func associate(sourceInstance sourceInstance: NSManagedObject, withDestinationInstance destinationInstance: NSManagedObject, for entityMapping: NSEntityMapping)
  @discardableResult
  func destinationInstances(forEntityMappingName mappingName: String, sourceInstances sourceInstances: [NSManagedObject]?) -> [NSManagedObject]
  @discardableResult
  func sourceInstances(forEntityMappingName mappingName: String, destinationInstances destinationInstances: [NSManagedObject]?) -> [NSManagedObject]
  var currentEntityMapping: NSEntityMapping { get }
  var migrationProgress: Float { get }
  var userInfo: [NSObject : AnyObject]?
  func cancelMigrationWithError(_ error: NSError)
}
struct _migrationManagerFlags {
  var _migrationWasCancelled: UInt32
  var _usesStoreSpecificMigrationManager: UInt32
  var _reservedMigrationManager: UInt32
  init()
  init(_migrationWasCancelled _migrationWasCancelled: UInt32, _usesStoreSpecificMigrationManager _usesStoreSpecificMigrationManager: UInt32, _reservedMigrationManager _reservedMigrationManager: UInt32)
}
