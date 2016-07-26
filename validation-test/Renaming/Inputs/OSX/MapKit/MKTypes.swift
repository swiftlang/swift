
@available(OSX 10.9, *)
enum MKMapType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case standard
  case satellite
  case hybrid
  @available(OSX 10.11, *)
  case satelliteFlyover
  @available(OSX 10.11, *)
  case hybridFlyover
}
let MKErrorDomain: String
@available(OSX 10.9, *)
enum MKErrorCode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknown
  case serverFailure
  case loadingThrottled
  case placemarkNotFound
  @available(OSX 10.9, *)
  case directionsNotFound
}
