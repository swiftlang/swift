
@available(OSX 10.11, *)
class NSBatchDeleteRequest : NSPersistentStoreRequest {
  init(fetchRequest fetch: NSFetchRequest)
  convenience init(objectIDs objects: [NSManagedObjectID])
  var resultType: NSBatchDeleteRequestResultType
  @NSCopying var fetchRequest: NSFetchRequest { get }
}
