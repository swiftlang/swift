
class NSURLCredentialStorage : NSObject {
  @discardableResult
  class func shared() -> NSURLCredentialStorage
  @discardableResult
  func credentials(for space: NSURLProtectionSpace) -> [String : NSURLCredential]?
  var allCredentials: [NSURLProtectionSpace : [String : NSURLCredential]] { get }
  func set(_ credential: NSURLCredential, for space: NSURLProtectionSpace)
  func remove(_ credential: NSURLCredential, for space: NSURLProtectionSpace)
  @available(OSX 10.9, *)
  func remove(_ credential: NSURLCredential, for space: NSURLProtectionSpace, options options: [String : AnyObject]? = [:])
  @discardableResult
  func defaultCredential(for space: NSURLProtectionSpace) -> NSURLCredential?
  func setDefaultCredential(_ credential: NSURLCredential, for space: NSURLProtectionSpace)
}
extension NSURLCredentialStorage {
  @available(OSX 10.10, *)
  func getCredentialsFor(_ protectionSpace: NSURLProtectionSpace, task task: NSURLSessionTask, completionHandler completionHandler: ([String : NSURLCredential]?) -> Void)
  @available(OSX 10.10, *)
  func setCredential(_ credential: NSURLCredential, for protectionSpace: NSURLProtectionSpace, task task: NSURLSessionTask)
  @available(OSX 10.10, *)
  func remove(_ credential: NSURLCredential, for protectionSpace: NSURLProtectionSpace, options options: [String : AnyObject]? = [:], task task: NSURLSessionTask)
  @available(OSX 10.10, *)
  func getDefaultCredential(for space: NSURLProtectionSpace, task task: NSURLSessionTask, completionHandler completionHandler: (NSURLCredential?) -> Void)
  @available(OSX 10.10, *)
  func setDefaultCredential(_ credential: NSURLCredential, for protectionSpace: NSURLProtectionSpace, task task: NSURLSessionTask)
}
let NSURLCredentialStorageChangedNotification: String
@available(OSX 10.9, *)
let NSURLCredentialStorageRemoveSynchronizableCredentials: String
