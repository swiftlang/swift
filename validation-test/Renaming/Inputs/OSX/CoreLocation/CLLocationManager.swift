
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
  case authorized
}
enum CLActivityType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case other
  case automotiveNavigation
  case fitness
  case otherNavigation
}
@available(OSX 10.6, *)
class CLLocationManager : NSObject {
  @available(OSX 10.7, *)
  @discardableResult
  class func locationServicesEnabled() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  class func headingAvailable() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  class func significantLocationChangeMonitoringAvailable() -> Bool
  @available(OSX 10.10, *)
  @discardableResult
  class func isMonitoringAvailable(for regionClass: AnyClass) -> Bool
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  @discardableResult
  class func regionMonitoringAvailable() -> Bool
  @available(OSX, introduced: 10.8, deprecated: 10.10)
  @discardableResult
  class func regionMonitoringEnabled() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  class func authorizationStatus() -> CLAuthorizationStatus
  unowned(unsafe) var delegate: @sil_unmanaged CLLocationManagerDelegate?
  @available(OSX 10.7, *)
  var purpose: String?
  var distanceFilter: CLLocationDistance
  var desiredAccuracy: CLLocationAccuracy
  @NSCopying var location: CLLocation? { get }
  @available(OSX 10.8, *)
  var maximumRegionMonitoringDistance: CLLocationDistance { get }
  @available(OSX 10.8, *)
  var monitoredRegions: Set<CLRegion> { get }
  func startUpdatingLocation()
  func stopUpdatingLocation()
  @available(OSX 10.7, *)
  func startMonitoringSignificantLocationChanges()
  @available(OSX 10.7, *)
  func stopMonitoringSignificantLocationChanges()
  @available(OSX 10.8, *)
  func stopMonitoring(for region: CLRegion)
  @available(OSX 10.8, *)
  func startMonitoring(for region: CLRegion)
  @available(OSX 10.10, *)
  func requestState(for region: CLRegion)
  @available(OSX 10.9, *)
  @discardableResult
  class func deferredLocationUpdatesAvailable() -> Bool
}
