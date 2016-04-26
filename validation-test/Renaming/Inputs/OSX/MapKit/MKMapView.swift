
@available(OSX 10.9, *)
class MKMapView : NSView, NSCoding {
  weak var delegate: @sil_weak MKMapViewDelegate?
  var mapType: MKMapType
  var region: MKCoordinateRegion
  func setRegion(_ region: MKCoordinateRegion, animated animated: Bool)
  var centerCoordinate: CLLocationCoordinate2D
  func setCenter(_ coordinate: CLLocationCoordinate2D, animated animated: Bool)
  @discardableResult
  func regionThatFits(_ region: MKCoordinateRegion) -> MKCoordinateRegion
  var visibleMapRect: MKMapRect
  func setVisibleMapRect(_ mapRect: MKMapRect, animated animate: Bool)
  @discardableResult
  func mapRectThatFits(_ mapRect: MKMapRect) -> MKMapRect
  func setVisibleMapRect(_ mapRect: MKMapRect, edgePadding insets: NSEdgeInsets, animated animate: Bool)
  @discardableResult
  func mapRectThatFits(_ mapRect: MKMapRect, edgePadding insets: NSEdgeInsets) -> MKMapRect
  @available(OSX 10.9, *)
  @NSCopying var camera: MKMapCamera
  @available(OSX 10.9, *)
  func setCamera(_ camera: MKMapCamera, animated animated: Bool)
  @discardableResult
  func convert(_ coordinate: CLLocationCoordinate2D, toPointTo view: NSView?) -> CGPoint
  @discardableResult
  func convert(_ point: CGPoint, toCoordinateFrom view: NSView?) -> CLLocationCoordinate2D
  @discardableResult
  func convertRegion(_ region: MKCoordinateRegion, toRectTo view: NSView?) -> CGRect
  @discardableResult
  func convert(_ rect: CGRect, toRegionFrom view: NSView?) -> MKCoordinateRegion
  var isZoomEnabled: Bool
  var isScrollEnabled: Bool
  @available(OSX 10.9, *)
  var isRotateEnabled: Bool
  @available(OSX 10.9, *)
  var isPitchEnabled: Bool
  @available(OSX 10.9, *)
  var showsZoomControls: Bool
  @available(OSX 10.9, *)
  var showsCompass: Bool
  @available(OSX 10.10, *)
  var showsScale: Bool
  @available(OSX 10.9, *)
  var showsPointsOfInterest: Bool
  @available(OSX 10.9, *)
  var showsBuildings: Bool
  @available(OSX 10.11, *)
  var showsTraffic: Bool
  var showsUserLocation: Bool
  var userLocation: MKUserLocation { get }
  var isUserLocationVisible: Bool { get }
  func addAnnotation(_ annotation: MKAnnotation)
  func addAnnotations(_ annotations: [MKAnnotation])
  func removeAnnotation(_ annotation: MKAnnotation)
  func removeAnnotations(_ annotations: [MKAnnotation])
  var annotations: [MKAnnotation] { get }
  @available(OSX 10.9, *)
  @discardableResult
  func annotations(in mapRect: MKMapRect) -> Set<NSObject>
  @discardableResult
  func view(for annotation: MKAnnotation) -> MKAnnotationView?
  @discardableResult
  func dequeueReusableAnnotationView(withIdentifier identifier: String) -> MKAnnotationView?
  func selectAnnotation(_ annotation: MKAnnotation, animated animated: Bool)
  func deselectAnnotation(_ annotation: MKAnnotation?, animated animated: Bool)
  var selectedAnnotations: [MKAnnotation]
  var annotationVisibleRect: CGRect { get }
  @available(OSX 10.9, *)
  func showAnnotations(_ annotations: [MKAnnotation], animated animated: Bool)
}
@available(OSX 10.9, *)
enum MKOverlayLevel : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case aboveRoads
  case aboveLabels
}
extension MKMapView {
  @available(OSX 10.9, *)
  func add(_ overlay: MKOverlay, level level: MKOverlayLevel)
  @available(OSX 10.9, *)
  func addOverlays(_ overlays: [MKOverlay], level level: MKOverlayLevel)
  @available(OSX 10.9, *)
  func remove(_ overlay: MKOverlay)
  @available(OSX 10.9, *)
  func removeOverlays(_ overlays: [MKOverlay])
  @available(OSX 10.9, *)
  func insert(_ overlay: MKOverlay, at index: Int, level level: MKOverlayLevel)
  @available(OSX 10.9, *)
  func insert(_ overlay: MKOverlay, above sibling: MKOverlay)
  @available(OSX 10.9, *)
  func insert(_ overlay: MKOverlay, below sibling: MKOverlay)
  @available(OSX 10.9, *)
  func exchangeOverlay(_ overlay1: MKOverlay, with overlay2: MKOverlay)
  @available(OSX 10.9, *)
  var overlays: [MKOverlay] { get }
  @available(OSX 10.9, *)
  @discardableResult
  func overlays(in level: MKOverlayLevel) -> [MKOverlay]
  @available(OSX 10.9, *)
  @discardableResult
  func renderer(for overlay: MKOverlay) -> MKOverlayRenderer?
  @available(OSX 10.9, *)
  func add(_ overlay: MKOverlay)
  @available(OSX 10.9, *)
  func addOverlays(_ overlays: [MKOverlay])
  @available(OSX 10.9, *)
  func insert(_ overlay: MKOverlay, at index: Int)
  @available(OSX 10.9, *)
  func exchangeOverlay(at index1: Int, withOverlayAt index2: Int)
}
protocol MKMapViewDelegate : NSObjectProtocol {
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, regionWillChangeAnimated animated: Bool)
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, regionDidChangeAnimated animated: Bool)
  @available(OSX 10.9, *)
  optional func mapViewWillStartLoadingMap(_ mapView: MKMapView)
  @available(OSX 10.9, *)
  optional func mapViewDidFinishLoadingMap(_ mapView: MKMapView)
  @available(OSX 10.9, *)
  optional func mapViewDidFailLoadingMap(_ mapView: MKMapView, withError error: NSError)
  @available(OSX 10.9, *)
  optional func mapViewWillStartRenderingMap(_ mapView: MKMapView)
  @available(OSX 10.9, *)
  optional func mapViewDidFinishRenderingMap(_ mapView: MKMapView, fullyRendered fullyRendered: Bool)
  @available(OSX 10.9, *)
  @discardableResult
  optional func mapView(_ mapView: MKMapView, viewFor annotation: MKAnnotation) -> MKAnnotationView?
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, didAdd views: [MKAnnotationView])
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, didSelect view: MKAnnotationView)
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, didDeselect view: MKAnnotationView)
  @available(OSX 10.9, *)
  optional func mapViewWillStartLocatingUser(_ mapView: MKMapView)
  @available(OSX 10.9, *)
  optional func mapViewDidStopLocatingUser(_ mapView: MKMapView)
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, didUpdate userLocation: MKUserLocation)
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, didFailToLocateUserWithError error: NSError)
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, annotationView view: MKAnnotationView, didChange newState: MKAnnotationViewDragState, fromOldState oldState: MKAnnotationViewDragState)
  @available(OSX 10.9, *)
  @discardableResult
  optional func mapView(_ mapView: MKMapView, rendererFor overlay: MKOverlay) -> MKOverlayRenderer
  @available(OSX 10.9, *)
  optional func mapView(_ mapView: MKMapView, didAdd renderers: [MKOverlayRenderer])
}
