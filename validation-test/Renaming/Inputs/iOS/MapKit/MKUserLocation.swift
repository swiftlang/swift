
@available(iOS 3.0, *)
class MKUserLocation : NSObject, MKAnnotation {
  var isUpdating: Bool { get }
  var location: CLLocation? { get }
  @available(iOS 5.0, *)
  var heading: CLHeading? { get }
}
