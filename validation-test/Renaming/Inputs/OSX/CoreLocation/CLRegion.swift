
@available(OSX 10.10, *)
enum CLRegionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case inside
  case outside
}
@available(OSX 10.7, *)
class CLRegion : NSObject, NSCopying, NSSecureCoding {
  @available(OSX, introduced: 10.7, deprecated: 10.10)
  init(circularRegionWithCenter center: CLLocationCoordinate2D, radius radius: CLLocationDistance, identifier identifier: String)
  @available(OSX, introduced: 10.7, deprecated: 10.10)
  var center: CLLocationCoordinate2D { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10)
  var radius: CLLocationDistance { get }
  @available(OSX 10.7, *)
  var identifier: String { get }
  @available(OSX 10.10, *)
  var notifyOnEntry: Bool
  @available(OSX 10.10, *)
  var notifyOnExit: Bool
  @available(OSX, introduced: 10.7, deprecated: 10.10)
  @discardableResult
  func contains(_ coordinate: CLLocationCoordinate2D) -> Bool
}
