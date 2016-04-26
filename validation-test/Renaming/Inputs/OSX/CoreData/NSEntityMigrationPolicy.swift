
@available(OSX 10.5, *)
let NSMigrationManagerKey: String
@available(OSX 10.5, *)
let NSMigrationSourceObjectKey: String
@available(OSX 10.5, *)
let NSMigrationDestinationObjectKey: String
@available(OSX 10.5, *)
let NSMigrationEntityMappingKey: String
@available(OSX 10.5, *)
let NSMigrationPropertyMappingKey: String
@available(OSX 10.5, *)
let NSMigrationEntityPolicyKey: String
@available(OSX 10.5, *)
class NSEntityMigrationPolicy : NSObject {
  func begin(_ mapping: NSEntityMapping, with manager: NSMigrationManager) throws
  func createDestinationInstances(forSource sInstance: NSManagedObject, in mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func endInstanceCreation(forMapping mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func createRelationships(forDestination dInstance: NSManagedObject, in mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func endRelationshipCreation(forMapping mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func performCustomValidation(forMapping mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
  func end(_ mapping: NSEntityMapping, manager manager: NSMigrationManager) throws
}
