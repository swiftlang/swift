
let TKErrorDomain: String
enum TKErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notImplemented
  case communicationError
  case corruptedData
  case canceledByUser
  case authenticationFailed
  case objectNotFound
  case tokenNotFound
  case badParameter
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Use TKErrorCodeAuthenticationFailed")
  static var TKErrorAuthenticationFailed: TKErrorCode { get }
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Use TKErrorCodeObjectNotFound")
  static var TKErrorObjectNotFound: TKErrorCode { get }
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Use TKErrorCodeTokenNotFound")
  static var TKErrorTokenNotFound: TKErrorCode { get }
}
