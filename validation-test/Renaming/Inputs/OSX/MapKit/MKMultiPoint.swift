
@available(OSX 10.9, *)
class MKMultiPoint : MKShape {
  @discardableResult
  func points() -> UnsafeMutablePointer<MKMapPoint>
  var pointCount: Int { get }
  func getCoordinates(_ coords: UnsafeMutablePointer<CLLocationCoordinate2D>, range range: NSRange)
}
