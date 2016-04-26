
@available(OSX 10.9, *)
class MKMapSnapshotOptions : NSObject, NSCopying {
  @NSCopying var camera: MKMapCamera
  var mapRect: MKMapRect
  var region: MKCoordinateRegion
  var mapType: MKMapType
  var showsPointsOfInterest: Bool
  var showsBuildings: Bool
  var size: NSSize
}
