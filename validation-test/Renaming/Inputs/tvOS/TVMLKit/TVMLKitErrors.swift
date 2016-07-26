
@available(tvOS 9.0, *)
let TVMLKitErrorDomain: String
@available(tvOS 9.0, *)
enum TVMLKitError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case internetUnavailable
  case failedToLaunch
  case last
}
