
typealias CLLocationDegrees = Double
typealias CLLocationAccuracy = Double
typealias CLLocationSpeed = Double
typealias CLLocationDirection = Double
struct CLLocationCoordinate2D {
  var latitude: CLLocationDegrees
  var longitude: CLLocationDegrees
  init()
  init(latitude latitude: CLLocationDegrees, longitude longitude: CLLocationDegrees)
}
typealias CLLocationDistance = Double
let kCLDistanceFilterNone: CLLocationDistance
@available(watchOS 2.0, *)
let kCLLocationAccuracyBestForNavigation: CLLocationAccuracy
let kCLLocationAccuracyBest: CLLocationAccuracy
let kCLLocationAccuracyNearestTenMeters: CLLocationAccuracy
let kCLLocationAccuracyHundredMeters: CLLocationAccuracy
let kCLLocationAccuracyKilometer: CLLocationAccuracy
let kCLLocationAccuracyThreeKilometers: CLLocationAccuracy
@available(watchOS 2.0, *)
let CLLocationDistanceMax: CLLocationDistance
@available(watchOS 2.0, *)
let CLTimeIntervalMax: NSTimeInterval
@available(watchOS 2.0, *)
let kCLLocationCoordinate2DInvalid: CLLocationCoordinate2D
@available(watchOS 2.0, *)
@discardableResult
func CLLocationCoordinate2DIsValid(_ coord: CLLocationCoordinate2D) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CLLocationCoordinate2DMake(_ latitude: CLLocationDegrees, _ longitude: CLLocationDegrees) -> CLLocationCoordinate2D
@available(watchOS 2.0, *)
class CLFloor : NSObject, NSCopying, NSSecureCoding {
  var level: Int { get }
}
@available(watchOS 2.0, *)
class CLLocation : NSObject, NSCopying, NSSecureCoding {
  init(latitude latitude: CLLocationDegrees, longitude longitude: CLLocationDegrees)
  init(coordinate coordinate: CLLocationCoordinate2D, altitude altitude: CLLocationDistance, horizontalAccuracy hAccuracy: CLLocationAccuracy, verticalAccuracy vAccuracy: CLLocationAccuracy, timestamp timestamp: NSDate)
  @available(watchOS 2.0, *)
  init(coordinate coordinate: CLLocationCoordinate2D, altitude altitude: CLLocationDistance, horizontalAccuracy hAccuracy: CLLocationAccuracy, verticalAccuracy vAccuracy: CLLocationAccuracy, course course: CLLocationDirection, speed speed: CLLocationSpeed, timestamp timestamp: NSDate)
  var coordinate: CLLocationCoordinate2D { get }
  var altitude: CLLocationDistance { get }
  var horizontalAccuracy: CLLocationAccuracy { get }
  var verticalAccuracy: CLLocationAccuracy { get }
  @NSCopying var timestamp: NSDate { get }
  @available(watchOS 2.0, *)
  @NSCopying var floor: CLFloor? { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func distance(from location: CLLocation) -> CLLocationDistance
}
