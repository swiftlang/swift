
enum CLDeviceOrientation : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case unknown
  case portrait
  case portraitUpsideDown
  case landscapeLeft
  case landscapeRight
  case faceUp
  case faceDown
}
enum CLAuthorizationStatus : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case notDetermined
  case restricted
  case denied
  @available(watchOS 2.0, *)
  case authorizedAlways
  @available(watchOS 2.0, *)
  case authorizedWhenInUse
}
enum CLActivityType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case other
  case automotiveNavigation
  case fitness
  case otherNavigation
}
@available(watchOS 2.0, *)
class CLLocationManager : NSObject {
  @available(watchOS 2.0, *)
  @discardableResult
  class func locationServicesEnabled() -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  class func authorizationStatus() -> CLAuthorizationStatus
  unowned(unsafe) var delegate: @sil_unmanaged CLLocationManagerDelegate?
  var distanceFilter: CLLocationDistance
  var desiredAccuracy: CLLocationAccuracy
  @NSCopying var location: CLLocation? { get }
  @available(watchOS 2.0, *)
  func requestWhenInUseAuthorization()
  @available(watchOS 2.0, *)
  func requestAlwaysAuthorization()
  func stopUpdatingLocation()
  @available(watchOS 2.0, *)
  func requestLocation()
}
