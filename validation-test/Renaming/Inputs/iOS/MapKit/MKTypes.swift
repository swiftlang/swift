
@available(iOS 3.0, *)
enum MKMapType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case standard
  case satellite
  case hybrid
  @available(iOS 9.0, *)
  case satelliteFlyover
  @available(iOS 9.0, *)
  case hybridFlyover
}
let MKErrorDomain: String
@available(iOS 3.0, *)
enum MKErrorCode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknown
  case serverFailure
  case loadingThrottled
  case placemarkNotFound
  @available(iOS 7.0, *)
  case directionsNotFound
}
