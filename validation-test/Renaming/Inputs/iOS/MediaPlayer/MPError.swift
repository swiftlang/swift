
@available(iOS 9.3, *)
let MPErrorDomain: String
@available(iOS 9.3, *)
enum MPErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case permissionDenied
  case cloudServiceCapabilityMissing
  case networkConnectionFailed
  case notFound
  case notSupported
}
