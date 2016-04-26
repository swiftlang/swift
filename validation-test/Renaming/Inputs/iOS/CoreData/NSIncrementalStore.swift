
@available(iOS 5.0, *)
class NSIncrementalStore : NSPersistentStore {
  @discardableResult
  func execute(_ request: NSPersistentStoreRequest, with context: NSManagedObjectContext?) throws -> AnyObject
  @discardableResult
  func newValuesForObject(with objectID: NSManagedObjectID, with context: NSManagedObjectContext) throws -> NSIncrementalStoreNode
  @discardableResult
  func newValue(forRelationship relationship: NSRelationshipDescription, forObjectWith objectID: NSManagedObjectID, with context: NSManagedObjectContext?) throws -> AnyObject
  @discardableResult
  class func identifierForNewStore(at storeURL: NSURL) -> AnyObject
  @discardableResult
  func obtainPermanentIDs(for array: [NSManagedObject]) throws -> [NSManagedObjectID]
  func managedObjectContextDidRegisterObjects(with objectIDs: [NSManagedObjectID])
  func managedObjectContextDidUnregisterObjects(with objectIDs: [NSManagedObjectID])
  @discardableResult
  func newObjectID(for entity: NSEntityDescription, referenceObject data: AnyObject) -> NSManagedObjectID
  @discardableResult
  func referenceObject(for objectID: NSManagedObjectID) -> AnyObject
}
