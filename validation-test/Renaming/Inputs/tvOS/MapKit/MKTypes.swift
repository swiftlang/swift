
@available(tvOS 9.2, *)
enum MKMapType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case standard
  case satellite
  case hybrid
  @available(tvOS 9.0, *)
  case satelliteFlyover
  @available(tvOS 9.0, *)
  case hybridFlyover
}
@available(tvOS 9.2, *)
let MKErrorDomain: String
@available(tvOS 9.2, *)
enum MKErrorCode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknown
  case serverFailure
  case loadingThrottled
  case placemarkNotFound
  @available(tvOS 7.0, *)
  case directionsNotFound
}
