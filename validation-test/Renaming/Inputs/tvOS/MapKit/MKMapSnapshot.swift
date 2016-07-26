
@available(tvOS 9.2, *)
class MKMapSnapshot : NSObject {
  var image: UIImage { get }
  @discardableResult
  func point(for coordinate: CLLocationCoordinate2D) -> CGPoint
}
