
class NSURLConnection : NSObject {
  @available(watchOS 2.0, *)
  @NSCopying var originalRequest: NSURLRequest { get }
  @available(watchOS 2.0, *)
  @NSCopying var currentRequest: NSURLRequest { get }
  @available(watchOS 2.0, *)
  func start()
  func cancel()
  @available(watchOS 2.0, *)
  func schedule(in aRunLoop: NSRunLoop, forMode mode: String)
  @available(watchOS 2.0, *)
  func unschedule(from aRunLoop: NSRunLoop, forMode mode: String)
  @available(watchOS 2.0, *)
  func setDelegateQueue(_ queue: NSOperationQueue?)
  @discardableResult
  class func canHandle(_ request: NSURLRequest) -> Bool
}
protocol NSURLConnectionDelegate : NSObjectProtocol {
  optional func connection(_ connection: NSURLConnection, didFailWithError error: NSError)
  @discardableResult
  optional func connectionShouldUseCredentialStorage(_ connection: NSURLConnection) -> Bool
  optional func connection(_ connection: NSURLConnection, willSendRequestFor challenge: NSURLAuthenticationChallenge)
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use -connection:willSendRequestForAuthenticationChallenge: instead.")
  @discardableResult
  optional func connection(_ connection: NSURLConnection, canAuthenticateAgainstProtectionSpace protectionSpace: NSURLProtectionSpace) -> Bool
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use -connection:willSendRequestForAuthenticationChallenge: instead.")
  optional func connection(_ connection: NSURLConnection, didReceive challenge: NSURLAuthenticationChallenge)
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use -connection:willSendRequestForAuthenticationChallenge: instead.")
  optional func connection(_ connection: NSURLConnection, didCancel challenge: NSURLAuthenticationChallenge)
}
protocol NSURLConnectionDataDelegate : NSURLConnectionDelegate {
  @discardableResult
  optional func connection(_ connection: NSURLConnection, willSend request: NSURLRequest, redirectResponse response: NSURLResponse?) -> NSURLRequest?
  optional func connection(_ connection: NSURLConnection, didReceive response: NSURLResponse)
  optional func connection(_ connection: NSURLConnection, didReceive data: NSData)
  @discardableResult
  optional func connection(_ connection: NSURLConnection, needNewBodyStream request: NSURLRequest) -> NSInputStream?
  optional func connection(_ connection: NSURLConnection, didSendBodyData bytesWritten: Int, totalBytesWritten totalBytesWritten: Int, totalBytesExpectedToWrite totalBytesExpectedToWrite: Int)
  @discardableResult
  optional func connection(_ connection: NSURLConnection, willCacheResponse cachedResponse: NSCachedURLResponse) -> NSCachedURLResponse?
  optional func connectionDidFinishLoading(_ connection: NSURLConnection)
}
protocol NSURLConnectionDownloadDelegate : NSURLConnectionDelegate {
  optional func connection(_ connection: NSURLConnection, didWriteData bytesWritten: Int64, totalBytesWritten totalBytesWritten: Int64, expectedTotalBytes expectedTotalBytes: Int64)
  optional func connectionDidResumeDownloading(_ connection: NSURLConnection, totalBytesWritten totalBytesWritten: Int64, expectedTotalBytes expectedTotalBytes: Int64)
  func connectionDidFinishDownloading(_ connection: NSURLConnection, destinationURL destinationURL: NSURL)
}
extension NSURLConnection {
}
extension NSURLConnection {
}
