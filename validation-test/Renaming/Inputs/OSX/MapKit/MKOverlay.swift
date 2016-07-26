
protocol MKOverlay : MKAnnotation {
  var boundingMapRect: MKMapRect { get }
  @discardableResult
  optional func intersects(_ mapRect: MKMapRect) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  optional func canReplaceMapContent() -> Bool
}
