
@available(tvOS 9.2, *)
class MKPolyline : MKMultiPoint, MKOverlay {
  convenience init(points points: UnsafeMutablePointer<MKMapPoint>, count count: Int)
  convenience init(coordinates coords: UnsafeMutablePointer<CLLocationCoordinate2D>, count count: Int)
}
