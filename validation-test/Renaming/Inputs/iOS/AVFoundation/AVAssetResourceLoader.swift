
@available(iOS 6.0, *)
class AVAssetResourceLoader : NSObject {
  func setDelegate(_ delegate: AVAssetResourceLoaderDelegate?, queue delegateQueue: dispatch_queue_t?)
  weak var delegate: @sil_weak AVAssetResourceLoaderDelegate? { get }
  var delegateQueue: dispatch_queue_t? { get }
}
protocol AVAssetResourceLoaderDelegate : NSObjectProtocol {
  @available(iOS 6.0, *)
  @discardableResult
  optional func resourceLoader(_ resourceLoader: AVAssetResourceLoader, shouldWaitForLoadingOfRequestedResource loadingRequest: AVAssetResourceLoadingRequest) -> Bool
  @available(iOS 8.0, *)
  @discardableResult
  optional func resourceLoader(_ resourceLoader: AVAssetResourceLoader, shouldWaitForRenewalOfRequestedResource renewalRequest: AVAssetResourceRenewalRequest) -> Bool
  @available(iOS 7.0, *)
  optional func resourceLoader(_ resourceLoader: AVAssetResourceLoader, didCancel loadingRequest: AVAssetResourceLoadingRequest)
  @available(iOS 8.0, *)
  @discardableResult
  optional func resourceLoader(_ resourceLoader: AVAssetResourceLoader, shouldWaitForResponseTo authenticationChallenge: NSURLAuthenticationChallenge) -> Bool
  @available(iOS 8.0, *)
  optional func resourceLoader(_ resourceLoader: AVAssetResourceLoader, didCancel authenticationChallenge: NSURLAuthenticationChallenge)
}
@available(iOS 6.0, *)
class AVAssetResourceLoadingRequest : NSObject {
  var request: NSURLRequest { get }
  var isFinished: Bool { get }
  @available(iOS 7.0, *)
  var isCancelled: Bool { get }
  @available(iOS 7.0, *)
  var contentInformationRequest: AVAssetResourceLoadingContentInformationRequest? { get }
  @available(iOS 7.0, *)
  var dataRequest: AVAssetResourceLoadingDataRequest? { get }
  @available(iOS 7.0, *)
  @NSCopying var response: NSURLResponse?
  @available(iOS 7.0, *)
  @NSCopying var redirect: NSURLRequest?
  @available(iOS 7.0, *)
  func finishLoading()
  func finishLoadingWithError(_ error: NSError?)
}
@available(iOS 8.0, *)
class AVAssetResourceRenewalRequest : AVAssetResourceLoadingRequest {
}
@available(iOS 7.0, *)
class AVAssetResourceLoadingContentInformationRequest : NSObject {
  var contentType: String?
  var contentLength: Int64
  var isByteRangeAccessSupported: Bool
  @available(iOS 8.0, *)
  @NSCopying var renewalDate: NSDate?
}
@available(iOS 7.0, *)
class AVAssetResourceLoadingDataRequest : NSObject {
  var requestedOffset: Int64 { get }
  var requestedLength: Int { get }
  @available(iOS 9.0, *)
  var requestsAllDataToEndOfResource: Bool { get }
  var currentOffset: Int64 { get }
  func respond(with data: NSData)
}
extension AVAssetResourceLoader {
  @available(iOS 9.0, *)
  var preloadsEligibleContentKeys: Bool
}
extension AVAssetResourceLoadingRequest {
  @discardableResult
  func streamingContentKeyRequestData(forApp appIdentifier: NSData, contentIdentifier contentIdentifier: NSData, options options: [String : AnyObject]? = [:]) throws -> NSData
  @available(iOS 9.0, *)
  @discardableResult
  func persistentContentKey(fromKeyVendorResponse keyVendorResponse: NSData, options options: [String : AnyObject]? = [:], error outError: NSErrorPointer) -> NSData
}
@available(iOS 9.0, *)
let AVAssetResourceLoadingRequestStreamingContentKeyRequestRequiresPersistentKey: String
extension AVAssetResourceLoadingRequest {
}
