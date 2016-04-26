
@available(iOS 8.2, *)
let WatchKitErrorDomain: String
@available(iOS 8.2, *)
enum WatchKitErrorCode : Int {
  case unknown
  case applicationDelegateWatchKitRequestReplyNotCalled
  case invalidArgument
  case mediaPlayerFailed
  case downloadFailed
  case recordingFailed
}

extension WatchKitErrorCode : _BridgedNSError {
}
