
@available(watchOS 2.0, *)
let NSMigrationManagerKey: String
@available(watchOS 2.0, *)
let NSMigrationSourceObjectKey: String
@available(watchOS 2.0, *)
let NSMigrationDestinationObjectKey: String
@available(watchOS 2.0, *)
let NSMigrationEntityMappingKey: String
@available(watchOS 2.0, *)
let NSMigrationPropertyMappingKey: String
@available(watchOS 2.0, *)
let NSMigrationEntityPolicyKey: String
@available(watchOS 2.0, *)
class NSEntityMigrationPolicy : NSObject {
  func begin(_ mapping: NSEntityMapping, with manager: NSMigrationManager) throws
  func createDestinationInstances(forSource sInstance: NSManagedObject, in mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func endInstanceCreation(forMapping mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func createRelationships(forDestination dInstance: NSManagedObject, in mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func endRelationshipCreation(forMapping mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func performCustomValidation(forMapping mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func end(_ mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
}
