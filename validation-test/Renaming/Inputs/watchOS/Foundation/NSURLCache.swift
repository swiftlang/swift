
enum NSURLCacheStoragePolicy : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case allowed
  case allowedInMemoryOnly
  case notAllowed
}
class NSCachedURLResponse : NSObject, NSSecureCoding, NSCopying {
  init(response response: NSURLResponse, data data: NSData)
  init(response response: NSURLResponse, data data: NSData, userInfo userInfo: [NSObject : AnyObject]? = [:], storagePolicy storagePolicy: NSURLCacheStoragePolicy)
  @NSCopying var response: NSURLResponse { get }
  @NSCopying var data: NSData { get }
  var userInfo: [NSObject : AnyObject]? { get }
  var storagePolicy: NSURLCacheStoragePolicy { get }
}
class NSURLCache : NSObject {
  @discardableResult
  class func shared() -> NSURLCache
  class func setSharedURLCache(_ cache: NSURLCache)
  init(memoryCapacity memoryCapacity: Int, diskCapacity diskCapacity: Int, diskPath path: String?)
  @discardableResult
  func cachedResponse(for request: NSURLRequest) -> NSCachedURLResponse?
  func storeCachedResponse(_ cachedResponse: NSCachedURLResponse, for request: NSURLRequest)
  func removeCachedResponse(for request: NSURLRequest)
  func removeAllCachedResponses()
  @available(watchOS 2.0, *)
  func removeCachedResponses(since date: NSDate)
  var memoryCapacity: Int
  var diskCapacity: Int
  var currentMemoryUsage: Int { get }
  var currentDiskUsage: Int { get }
}
extension NSURLCache {
  @available(watchOS 2.0, *)
  func storeCachedResponse(_ cachedResponse: NSCachedURLResponse, for dataTask: NSURLSessionDataTask)
  @available(watchOS 2.0, *)
  func getCachedResponse(for dataTask: NSURLSessionDataTask, completionHandler completionHandler: (NSCachedURLResponse?) -> Void)
  @available(watchOS 2.0, *)
  func removeCachedResponse(for dataTask: NSURLSessionDataTask)
}
