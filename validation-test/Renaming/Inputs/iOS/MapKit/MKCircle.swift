
@available(iOS 4.0, *)
class MKCircle : MKShape, MKOverlay {
  convenience init(center coord: CLLocationCoordinate2D, radius radius: CLLocationDistance)
  convenience init(mapRect mapRect: MKMapRect)
  var radius: CLLocationDistance { get }
}
