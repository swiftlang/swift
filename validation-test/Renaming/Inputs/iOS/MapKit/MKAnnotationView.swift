
let MKAnnotationCalloutInfoDidChangeNotification: String
@available(iOS 4.0, *)
enum MKAnnotationViewDragState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case starting
  case dragging
  case canceling
  case ending
}
@available(iOS 3.0, *)
class MKAnnotationView : UIView {
  init(annotation annotation: MKAnnotation?, reuseIdentifier reuseIdentifier: String?)
  var reuseIdentifier: String? { get }
  func prepareForReuse()
  var annotation: MKAnnotation?
  var image: UIImage?
  var centerOffset: CGPoint
  var calloutOffset: CGPoint
  var isEnabled: Bool
  var isHighlighted: Bool
  var isSelected: Bool
  func setSelected(_ selected: Bool, animated animated: Bool)
  var canShowCallout: Bool
  var leftCalloutAccessoryView: UIView?
  var rightCalloutAccessoryView: UIView?
  @available(iOS 9.0, *)
  var detailCalloutAccessoryView: UIView?
  @available(iOS 4.0, *)
  var isDraggable: Bool
  @available(iOS 4.0, *)
  var dragState: MKAnnotationViewDragState
  @available(iOS 4.2, *)
  func setDragState(_ newDragState: MKAnnotationViewDragState, animated animated: Bool)
}
