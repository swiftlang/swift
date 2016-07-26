
@available(tvOS 5.0, *)
class NSSaveChangesRequest : NSPersistentStoreRequest {
  init(inserted insertedObjects: Set<NSManagedObject>?, updated updatedObjects: Set<NSManagedObject>?, deleted deletedObjects: Set<NSManagedObject>?, locked lockedObjects: Set<NSManagedObject>?)
  var insertedObjects: Set<NSManagedObject>? { get }
  var updatedObjects: Set<NSManagedObject>? { get }
  var deletedObjects: Set<NSManagedObject>? { get }
  var lockedObjects: Set<NSManagedObject>? { get }
}
