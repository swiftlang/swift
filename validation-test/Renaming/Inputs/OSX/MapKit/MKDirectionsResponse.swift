
@available(OSX 10.9, *)
class MKDirectionsResponse : NSObject {
  var source: MKMapItem { get }
  var destination: MKMapItem { get }
  var routes: [MKRoute] { get }
}
@available(OSX 10.9, *)
class MKRoute : NSObject {
  var name: String { get }
  var advisoryNotices: [String] { get }
  var distance: CLLocationDistance { get }
  var expectedTravelTime: NSTimeInterval { get }
  var transportType: MKDirectionsTransportType { get }
  var polyline: MKPolyline { get }
  var steps: [MKRouteStep] { get }
}
@available(OSX 10.9, *)
class MKRouteStep : NSObject {
  var instructions: String { get }
  var notice: String? { get }
  var polyline: MKPolyline { get }
  var distance: CLLocationDistance { get }
  var transportType: MKDirectionsTransportType { get }
}
@available(OSX 10.9, *)
class MKETAResponse : NSObject {
  var source: MKMapItem { get }
  var destination: MKMapItem { get }
  var expectedTravelTime: NSTimeInterval { get }
  @available(OSX 10.11, *)
  var distance: CLLocationDistance { get }
  @available(OSX 10.11, *)
  var expectedArrivalDate: NSDate { get }
  @available(OSX 10.11, *)
  var expectedDepartureDate: NSDate { get }
  @available(OSX 10.11, *)
  var transportType: MKDirectionsTransportType { get }
}
