
@available(tvOS 6.0, *)
enum SKDownloadState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case waiting
  case active
  case paused
  case finished
  case failed
  case cancelled
}
@available(tvOS 6.0, *)
var SKDownloadTimeRemainingUnknown: NSTimeInterval
@available(tvOS 6.0, *)
class SKDownload : NSObject {
  @available(tvOS 6.0, *)
  var downloadState: SKDownloadState { get }
  @available(tvOS 6.0, *)
  var contentLength: Int64 { get }
  @available(tvOS 6.0, *)
  var contentIdentifier: String { get }
  @available(tvOS 6.0, *)
  var contentURL: NSURL? { get }
  @available(tvOS 6.0, *)
  var contentVersion: String { get }
  @available(tvOS 6.0, *)
  var error: NSError? { get }
  @available(tvOS 6.0, *)
  var progress: Float { get }
  @available(tvOS 6.0, *)
  var timeRemaining: NSTimeInterval { get }
  @available(tvOS 6.0, *)
  var transaction: SKPaymentTransaction { get }
}
