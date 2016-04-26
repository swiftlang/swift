
protocol MKOverlay : MKAnnotation {
  var boundingMapRect: MKMapRect { get }
  @discardableResult
  optional func intersects(_ mapRect: MKMapRect) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  optional func canReplaceMapContent() -> Bool
}
