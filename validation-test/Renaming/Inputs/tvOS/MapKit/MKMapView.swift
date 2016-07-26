
@available(tvOS 9.2, *)
enum MKUserTrackingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case follow
}
@available(tvOS 9.2, *)
class MKMapView : UIView, NSCoding {
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
  func setVisibleMapRect(_ mapRect: MKMapRect, edgePadding insets: UIEdgeInsets, animated animate: Bool)
  @discardableResult
  func mapRectThatFits(_ mapRect: MKMapRect, edgePadding insets: UIEdgeInsets) -> MKMapRect
  @available(tvOS 7.0, *)
  @NSCopying var camera: MKMapCamera
  @available(tvOS 7.0, *)
  func setCamera(_ camera: MKMapCamera, animated animated: Bool)
  @discardableResult
  func convert(_ coordinate: CLLocationCoordinate2D, toPointTo view: UIView?) -> CGPoint
  @discardableResult
  func convert(_ point: CGPoint, toCoordinateFrom view: UIView?) -> CLLocationCoordinate2D
  @discardableResult
  func convertRegion(_ region: MKCoordinateRegion, toRectTo view: UIView?) -> CGRect
  @discardableResult
  func convert(_ rect: CGRect, toRegionFrom view: UIView?) -> MKCoordinateRegion
  var isZoomEnabled: Bool
  var isScrollEnabled: Bool
  @available(tvOS 9.0, *)
  var showsScale: Bool
  @available(tvOS 7.0, *)
  var showsPointsOfInterest: Bool
  @available(tvOS 7.0, *)
  var showsBuildings: Bool
  @available(tvOS 9.0, *)
  var showsTraffic: Bool
  var showsUserLocation: Bool
  var userLocation: MKUserLocation { get }
  @available(tvOS 5.0, *)
  var userTrackingMode: MKUserTrackingMode
  @available(tvOS 5.0, *)
  func setUserTrackingMode(_ mode: MKUserTrackingMode, animated animated: Bool)
  var isUserLocationVisible: Bool { get }
  func addAnnotation(_ annotation: MKAnnotation)
  func addAnnotations(_ annotations: [MKAnnotation])
  func removeAnnotation(_ annotation: MKAnnotation)
  func removeAnnotations(_ annotations: [MKAnnotation])
  var annotations: [MKAnnotation] { get }
  @available(tvOS 4.2, *)
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
  @available(tvOS 7.0, *)
  func showAnnotations(_ annotations: [MKAnnotation], animated animated: Bool)
}
@available(tvOS 9.2, *)
enum MKOverlayLevel : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case aboveRoads
  case aboveLabels
}
extension MKMapView {
  @available(tvOS 7.0, *)
  func add(_ overlay: MKOverlay, level level: MKOverlayLevel)
  @available(tvOS 7.0, *)
  func addOverlays(_ overlays: [MKOverlay], level level: MKOverlayLevel)
  @available(tvOS 4.0, *)
  func remove(_ overlay: MKOverlay)
  @available(tvOS 4.0, *)
  func removeOverlays(_ overlays: [MKOverlay])
  @available(tvOS 7.0, *)
  func insert(_ overlay: MKOverlay, at index: Int, level level: MKOverlayLevel)
  @available(tvOS 4.0, *)
  func insert(_ overlay: MKOverlay, above sibling: MKOverlay)
  @available(tvOS 4.0, *)
  func insert(_ overlay: MKOverlay, below sibling: MKOverlay)
  @available(tvOS 7.0, *)
  func exchangeOverlay(_ overlay1: MKOverlay, with overlay2: MKOverlay)
  @available(tvOS 4.0, *)
  var overlays: [MKOverlay] { get }
  @available(tvOS 7.0, *)
  @discardableResult
  func overlays(in level: MKOverlayLevel) -> [MKOverlay]
  @available(tvOS 7.0, *)
  @discardableResult
  func renderer(for overlay: MKOverlay) -> MKOverlayRenderer?
  @available(tvOS 4.0, *)
  func add(_ overlay: MKOverlay)
  @available(tvOS 4.0, *)
  func addOverlays(_ overlays: [MKOverlay])
  @available(tvOS 4.0, *)
  func insert(_ overlay: MKOverlay, at index: Int)
  @available(tvOS 4.0, *)
  func exchangeOverlay(at index1: Int, withOverlayAt index2: Int)
}
protocol MKMapViewDelegate : NSObjectProtocol {
  @available(tvOS 9.2, *)
  optional func mapView(_ mapView: MKMapView, regionWillChangeAnimated animated: Bool)
  @available(tvOS 9.2, *)
  optional func mapView(_ mapView: MKMapView, regionDidChangeAnimated animated: Bool)
  @available(tvOS 9.2, *)
  optional func mapViewWillStartLoadingMap(_ mapView: MKMapView)
  @available(tvOS 9.2, *)
  optional func mapViewDidFinishLoadingMap(_ mapView: MKMapView)
  @available(tvOS 9.2, *)
  optional func mapViewDidFailLoadingMap(_ mapView: MKMapView, withError error: NSError)
  @available(tvOS 7.0, *)
  optional func mapViewWillStartRenderingMap(_ mapView: MKMapView)
  @available(tvOS 7.0, *)
  optional func mapViewDidFinishRenderingMap(_ mapView: MKMapView, fullyRendered fullyRendered: Bool)
  @available(tvOS 9.2, *)
  @discardableResult
  optional func mapView(_ mapView: MKMapView, viewFor annotation: MKAnnotation) -> MKAnnotationView?
  @available(tvOS 9.2, *)
  optional func mapView(_ mapView: MKMapView, didAdd views: [MKAnnotationView])
  @available(tvOS 4.0, *)
  optional func mapView(_ mapView: MKMapView, didSelect view: MKAnnotationView)
  @available(tvOS 4.0, *)
  optional func mapView(_ mapView: MKMapView, didDeselect view: MKAnnotationView)
  @available(tvOS 4.0, *)
  optional func mapViewWillStartLocatingUser(_ mapView: MKMapView)
  @available(tvOS 4.0, *)
  optional func mapViewDidStopLocatingUser(_ mapView: MKMapView)
  @available(tvOS 4.0, *)
  optional func mapView(_ mapView: MKMapView, didUpdate userLocation: MKUserLocation)
  @available(tvOS 4.0, *)
  optional func mapView(_ mapView: MKMapView, didFailToLocateUserWithError error: NSError)
  @available(tvOS 5.0, *)
  optional func mapView(_ mapView: MKMapView, didChange mode: MKUserTrackingMode, animated animated: Bool)
  @available(tvOS 7.0, *)
  @discardableResult
  optional func mapView(_ mapView: MKMapView, rendererFor overlay: MKOverlay) -> MKOverlayRenderer
  @available(tvOS 7.0, *)
  optional func mapView(_ mapView: MKMapView, didAdd renderers: [MKOverlayRenderer])
}
