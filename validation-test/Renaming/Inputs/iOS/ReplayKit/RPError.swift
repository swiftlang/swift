
let RPRecordingErrorDomain: String
@available(iOS 9.0, *)
enum RPRecordingErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case userDeclined
  case disabled
  case failedToStart
  case failed
  case insufficientStorage
  case interrupted
  case contentResize
}
