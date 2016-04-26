
@available(OSX 10.10, *)
class NSBatchUpdateRequest : NSPersistentStoreRequest {
  init(entityName entityName: String)
  init(entity entity: NSEntityDescription)
  var entityName: String { get }
  var entity: NSEntityDescription { get }
  var predicate: NSPredicate?
  var includesSubentities: Bool
  var resultType: NSBatchUpdateRequestResultType
  var propertiesToUpdate: [NSObject : AnyObject]?
}
struct _requestFlags {
  var includesSubentities: UInt32
  var resultType: UInt32
  var entityIsName: UInt32
  var _RESERVED: UInt32
  init()
  init(includesSubentities includesSubentities: UInt32, resultType resultType: UInt32, entityIsName entityIsName: UInt32, _RESERVED _RESERVED: UInt32)
}
