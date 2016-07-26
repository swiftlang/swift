
@available(iOS 7.0, *)
class MKOverlayRenderer : NSObject {
  init(overlay overlay: MKOverlay)
  var overlay: MKOverlay { get }
  @discardableResult
  func point(for mapPoint: MKMapPoint) -> CGPoint
  @discardableResult
  func mapPoint(for point: CGPoint) -> MKMapPoint
  @discardableResult
  func rect(for mapRect: MKMapRect) -> CGRect
  @discardableResult
  func mapRect(for rect: CGRect) -> MKMapRect
  @discardableResult
  func canDraw(_ mapRect: MKMapRect, zoomScale zoomScale: MKZoomScale) -> Bool
  func draw(_ mapRect: MKMapRect, zoomScale zoomScale: MKZoomScale, in context: CGContext)
  func setNeedsDisplay()
  func setNeedsDisplayIn(_ mapRect: MKMapRect)
  func setNeedsDisplayIn(_ mapRect: MKMapRect, zoomScale zoomScale: MKZoomScale)
  var alpha: CGFloat
  var contentScaleFactor: CGFloat { get }
}
