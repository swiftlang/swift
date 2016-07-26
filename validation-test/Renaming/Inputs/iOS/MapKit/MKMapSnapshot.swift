
@available(iOS 7.0, *)
class MKMapSnapshot : NSObject {
  var image: UIImage { get }
  @discardableResult
  func point(for coordinate: CLLocationCoordinate2D) -> CGPoint
}
