
@available(iOS 5.0, *)
class UIManagedDocument : UIDocument {
  @discardableResult
  class func persistentStoreName() -> String
  var persistentStoreOptions: [NSObject : AnyObject]?
  var modelConfiguration: String?
  func configurePersistentStoreCoordinator(for storeURL: NSURL, ofType fileType: String, modelConfiguration configuration: String?, storeOptions storeOptions: [NSObject : AnyObject]? = [:]) throws
  @discardableResult
  func persistentStoreType(forFileType fileType: String) -> String
  func readAdditionalContent(from absoluteURL: NSURL) throws
  @discardableResult
  func additionalContent(for absoluteURL: NSURL) throws -> AnyObject
  func writeAdditionalContent(_ content: AnyObject, to absoluteURL: NSURL, originalContentsURL absoluteOriginalContentsURL: NSURL?) throws
}
