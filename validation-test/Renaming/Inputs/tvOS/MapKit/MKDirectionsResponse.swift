
@available(tvOS 9.2, *)
class MKDirectionsResponse : NSObject {
  var source: MKMapItem { get }
  var destination: MKMapItem { get }
  var routes: [MKRoute] { get }
}
@available(tvOS 9.2, *)
class MKRoute : NSObject {
  var name: String { get }
  var advisoryNotices: [String] { get }
  var distance: CLLocationDistance { get }
  var expectedTravelTime: NSTimeInterval { get }
  var transportType: MKDirectionsTransportType { get }
  var polyline: MKPolyline { get }
  var steps: [MKRouteStep] { get }
}
@available(tvOS 9.2, *)
class MKRouteStep : NSObject {
  var instructions: String { get }
  var notice: String? { get }
  var polyline: MKPolyline { get }
  var distance: CLLocationDistance { get }
  var transportType: MKDirectionsTransportType { get }
}
@available(tvOS 9.2, *)
class MKETAResponse : NSObject {
  var source: MKMapItem { get }
  var destination: MKMapItem { get }
  var expectedTravelTime: NSTimeInterval { get }
  @available(tvOS 9.0, *)
  var distance: CLLocationDistance { get }
  @available(tvOS 9.0, *)
  var expectedArrivalDate: NSDate { get }
  @available(tvOS 9.0, *)
  var expectedDepartureDate: NSDate { get }
  @available(tvOS 9.0, *)
  var transportType: MKDirectionsTransportType { get }
}
