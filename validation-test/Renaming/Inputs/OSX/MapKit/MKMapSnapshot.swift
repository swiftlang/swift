
@available(OSX 10.9, *)
class MKMapSnapshot : NSObject {
  var image: NSImage { get }
  @discardableResult
  func point(for coordinate: CLLocationCoordinate2D) -> NSPoint
}
