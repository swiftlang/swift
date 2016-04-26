
class NSURLCredentialStorage : NSObject {
  @discardableResult
  class func shared() -> NSURLCredentialStorage
  @discardableResult
  func credentials(for space: NSURLProtectionSpace) -> [String : NSURLCredential]?
  var allCredentials: [NSURLProtectionSpace : [String : NSURLCredential]] { get }
  func set(_ credential: NSURLCredential, for space: NSURLProtectionSpace)
  func remove(_ credential: NSURLCredential, for space: NSURLProtectionSpace)
  @available(watchOS 2.0, *)
  func remove(_ credential: NSURLCredential, for space: NSURLProtectionSpace, options options: [String : AnyObject]? = [:])
  @discardableResult
  func defaultCredential(for space: NSURLProtectionSpace) -> NSURLCredential?
  func setDefaultCredential(_ credential: NSURLCredential, for space: NSURLProtectionSpace)
}
extension NSURLCredentialStorage {
  @available(watchOS 2.0, *)
  func getCredentialsFor(_ protectionSpace: NSURLProtectionSpace, task task: NSURLSessionTask, completionHandler completionHandler: ([String : NSURLCredential]?) -> Void)
  @available(watchOS 2.0, *)
  func setCredential(_ credential: NSURLCredential, for protectionSpace: NSURLProtectionSpace, task task: NSURLSessionTask)
  @available(watchOS 2.0, *)
  func remove(_ credential: NSURLCredential, for protectionSpace: NSURLProtectionSpace, options options: [String : AnyObject]? = [:], task task: NSURLSessionTask)
  @available(watchOS 2.0, *)
  func getDefaultCredential(for space: NSURLProtectionSpace, task task: NSURLSessionTask, completionHandler completionHandler: (NSURLCredential?) -> Void)
  @available(watchOS 2.0, *)
  func setDefaultCredential(_ credential: NSURLCredential, for protectionSpace: NSURLProtectionSpace, task task: NSURLSessionTask)
}
let NSURLCredentialStorageChangedNotification: String
@available(watchOS 2.0, *)
let NSURLCredentialStorageRemoveSynchronizableCredentials: String
