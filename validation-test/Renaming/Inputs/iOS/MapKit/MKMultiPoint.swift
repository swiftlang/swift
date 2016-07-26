
@available(iOS 4.0, *)
class MKMultiPoint : MKShape {
  @discardableResult
  func points() -> UnsafeMutablePointer<MKMapPoint>
  var pointCount: Int { get }
  func getCoordinates(_ coords: UnsafeMutablePointer<CLLocationCoordinate2D>, range range: NSRange)
}
