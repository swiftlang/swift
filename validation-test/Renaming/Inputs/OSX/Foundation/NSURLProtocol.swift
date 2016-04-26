
protocol NSURLProtocolClient : NSObjectProtocol {
  func urlProtocol(_ protocol: NSURLProtocol, wasRedirectedTo request: NSURLRequest, redirectResponse redirectResponse: NSURLResponse)
  func urlProtocol(_ protocol: NSURLProtocol, cachedResponseIsValid cachedResponse: NSCachedURLResponse)
  func urlProtocol(_ protocol: NSURLProtocol, didReceive response: NSURLResponse, cacheStoragePolicy policy: NSURLCacheStoragePolicy)
  func urlProtocol(_ protocol: NSURLProtocol, didLoad data: NSData)
  func urlProtocolDidFinishLoading(_ protocol: NSURLProtocol)
  func urlProtocol(_ protocol: NSURLProtocol, didFailWithError error: NSError)
  func urlProtocol(_ protocol: NSURLProtocol, didReceive challenge: NSURLAuthenticationChallenge)
  func urlProtocol(_ protocol: NSURLProtocol, didCancel challenge: NSURLAuthenticationChallenge)
}
class NSURLProtocol : NSObject {
  init(request request: NSURLRequest, cachedResponse cachedResponse: NSCachedURLResponse?, client client: NSURLProtocolClient?)
  var client: NSURLProtocolClient? { get }
  @NSCopying var request: NSURLRequest { get }
  @NSCopying var cachedResponse: NSCachedURLResponse? { get }
  @discardableResult
  class func canInit(with request: NSURLRequest) -> Bool
  @discardableResult
  class func canonicalRequest(for request: NSURLRequest) -> NSURLRequest
  @discardableResult
  class func requestIsCacheEquivalent(_ a: NSURLRequest, to b: NSURLRequest) -> Bool
  func startLoading()
  func stopLoading()
  @discardableResult
  class func property(forKey key: String, in request: NSURLRequest) -> AnyObject?
  class func setProperty(_ value: AnyObject, forKey key: String, in request: NSMutableURLRequest)
  class func removeProperty(forKey key: String, in request: NSMutableURLRequest)
  @discardableResult
  class func registerClass(_ protocolClass: AnyClass) -> Bool
  class func unregisterClass(_ protocolClass: AnyClass)
}
extension NSURLProtocol {
  @available(OSX 10.10, *)
  @discardableResult
  class func canInit(with task: NSURLSessionTask) -> Bool
  @available(OSX 10.10, *)
  convenience init(task task: NSURLSessionTask, cachedResponse cachedResponse: NSCachedURLResponse?, client client: NSURLProtocolClient?)
  @available(OSX 10.10, *)
  @NSCopying var task: NSURLSessionTask? { get }
}
