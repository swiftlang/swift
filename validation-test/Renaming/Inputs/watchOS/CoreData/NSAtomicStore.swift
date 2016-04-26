
@available(watchOS 2.0, *)
class NSAtomicStore : NSPersistentStore {
  func load() throws
  func save() throws
  @discardableResult
  func newCacheNode(for managedObject: NSManagedObject) -> NSAtomicStoreCacheNode
  func updateCacheNode(_ node: NSAtomicStoreCacheNode, from managedObject: NSManagedObject)
  @discardableResult
  func cacheNodes() -> Set<NSAtomicStoreCacheNode>
  func addCacheNodes(_ cacheNodes: Set<NSAtomicStoreCacheNode>)
  func willRemoveCacheNodes(_ cacheNodes: Set<NSAtomicStoreCacheNode>)
  @discardableResult
  func cacheNode(for objectID: NSManagedObjectID) -> NSAtomicStoreCacheNode?
  @discardableResult
  func objectID(for entity: NSEntityDescription, withReferenceObject data: AnyObject) -> NSManagedObjectID
  @discardableResult
  func newReferenceObject(for managedObject: NSManagedObject) -> AnyObject
  @discardableResult
  func referenceObject(for objectID: NSManagedObjectID) -> AnyObject
}
