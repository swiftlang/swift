import CoreData

// <rdar://problem/16879162>
@public class OtherManagedObject : NSManagedObject {
  @NSManaged @public var managed: String
}

@public func getMyManagedObject() -> MyManagedObject {
  return MyManagedObject()
}
