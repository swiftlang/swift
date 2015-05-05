import CoreData

// <rdar://problem/16879162>
class OtherManagedObject : NSManagedObject {
  @NSManaged var managed: String
}

func getMyManagedObject() -> MyManagedObject {
  return MyManagedObject()
}

extension MyManagedObject {
  @NSManaged var anotherManaged: String
}
