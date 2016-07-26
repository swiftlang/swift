
struct SKDownloadState : RawRepresentable, Equatable {
  init(_ rawValue: Int)
  init(rawValue rawValue: Int)
  var rawValue: Int
}
var SKDownloadStateWaiting: SKDownloadState { get }
var SKDownloadStateActive: SKDownloadState { get }
var SKDownloadStatePaused: SKDownloadState { get }
var SKDownloadStateFinished: SKDownloadState { get }
var SKDownloadStateFailed: SKDownloadState { get }
var SKDownloadStateCancelled: SKDownloadState { get }
@available(OSX 10.8, *)
class SKDownload : NSObject {
  var contentIdentifier: String { get }
  var state: SKDownloadState { get }
  @NSCopying var contentURL: NSURL? { get }
  var progress: Float { get }
  @NSCopying var error: NSError? { get }
  var timeRemaining: NSTimeInterval { get }
  @NSCopying var contentLength: NSNumber { get }
  var contentVersion: String? { get }
  var transaction: SKPaymentTransaction? { get }
  @discardableResult
  class func contentURL(forProductID productID: String) -> NSURL?
  class func deleteContent(forProductID productID: String)
}
