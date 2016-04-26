
enum NSURLHandleStatus : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case notLoaded
  case loadSucceeded
  case loadInProgress
  case loadFailed
}
protocol NSURLHandleClient {
}
class NSURLHandle : NSObject {
}
