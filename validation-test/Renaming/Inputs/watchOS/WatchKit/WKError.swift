
@available(watchOS 2.0, *)
let WatchKitErrorDomain: String
@available(watchOS 2.0, *)
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
