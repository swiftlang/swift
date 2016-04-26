
class NSPersistentDocument : NSDocument {
  var managedObjectContext: NSManagedObjectContext?
  var managedObjectModel: NSManagedObjectModel? { get }
  @available(OSX 10.5, *)
  func configurePersistentStoreCoordinator(for url: NSURL, ofType fileType: String, modelConfiguration configuration: String?, storeOptions storeOptions: [String : AnyObject]? = [:]) throws
  @discardableResult
  func persistentStoreType(forFileType fileType: String) -> String
}
extension NSPersistentDocument {
}
