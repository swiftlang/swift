
@available(watchOS 2.0, *)
class NSBatchDeleteRequest : NSPersistentStoreRequest {
  init(fetchRequest fetch: NSFetchRequest)
  convenience init(objectIDs objects: [NSManagedObjectID])
  var resultType: NSBatchDeleteRequestResultType
  @NSCopying var fetchRequest: NSFetchRequest { get }
}
