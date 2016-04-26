
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
  @available(iOS 8.0, *)
  case authorizedAlways
  @available(iOS 8.0, *)
  case authorizedWhenInUse
  @available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use kCLAuthorizationStatusAuthorizedAlways")
  static var authorized: CLAuthorizationStatus { get }
}
enum CLActivityType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case other
  case automotiveNavigation
  case fitness
  case otherNavigation
}
@available(iOS 2.0, *)
class CLLocationManager : NSObject {
  @available(iOS 4.0, *)
  @discardableResult
  class func locationServicesEnabled() -> Bool
  @available(iOS 4.0, *)
  @discardableResult
  class func headingAvailable() -> Bool
  @available(iOS 4.0, *)
  @discardableResult
  class func significantLocationChangeMonitoringAvailable() -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  class func isMonitoringAvailable(for regionClass: AnyClass) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  class func isRangingAvailable() -> Bool
  @available(iOS 4.2, *)
  @discardableResult
  class func authorizationStatus() -> CLAuthorizationStatus
  unowned(unsafe) var delegate: @sil_unmanaged CLLocationManagerDelegate?
  @available(iOS 6.0, *)
  var activityType: CLActivityType
  var distanceFilter: CLLocationDistance
  var desiredAccuracy: CLLocationAccuracy
  @available(iOS 6.0, *)
  var pausesLocationUpdatesAutomatically: Bool
  @available(iOS 9.0, *)
  var allowsBackgroundLocationUpdates: Bool
  @NSCopying var location: CLLocation? { get }
  @available(iOS 3.0, *)
  var headingFilter: CLLocationDegrees
  @available(iOS 4.0, *)
  var headingOrientation: CLDeviceOrientation
  @available(iOS 4.0, *)
  @NSCopying var heading: CLHeading? { get }
  @available(iOS 4.0, *)
  var maximumRegionMonitoringDistance: CLLocationDistance { get }
  @available(iOS 4.0, *)
  var monitoredRegions: Set<CLRegion> { get }
  @available(iOS 7.0, *)
  var rangedRegions: Set<CLRegion> { get }
  @available(iOS 8.0, *)
  func requestWhenInUseAuthorization()
  @available(iOS 8.0, *)
  func requestAlwaysAuthorization()
  func startUpdatingLocation()
  func stopUpdatingLocation()
  @available(iOS 9.0, *)
  func requestLocation()
  @available(iOS 3.0, *)
  func startUpdatingHeading()
  @available(iOS 3.0, *)
  func stopUpdatingHeading()
  @available(iOS 3.0, *)
  func dismissHeadingCalibrationDisplay()
  @available(iOS 4.0, *)
  func startMonitoringSignificantLocationChanges()
  @available(iOS 4.0, *)
  func stopMonitoringSignificantLocationChanges()
  @available(iOS 4.0, *)
  func stopMonitoring(for region: CLRegion)
  @available(iOS 5.0, *)
  func startMonitoring(for region: CLRegion)
  @available(iOS 7.0, *)
  func requestState(for region: CLRegion)
  @available(iOS 7.0, *)
  func startRangingBeacons(in region: CLBeaconRegion)
  @available(iOS 7.0, *)
  func stopRangingBeacons(in region: CLBeaconRegion)
  @available(iOS 6.0, *)
  func allowDeferredLocationUpdates(untilTraveled distance: CLLocationDistance, timeout timeout: NSTimeInterval)
  @available(iOS 6.0, *)
  func disallowDeferredLocationUpdates()
  @available(iOS 6.0, *)
  @discardableResult
  class func deferredLocationUpdatesAvailable() -> Bool
}
