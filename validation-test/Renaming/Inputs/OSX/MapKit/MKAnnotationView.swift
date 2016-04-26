
let MKAnnotationCalloutInfoDidChangeNotification: String
@available(OSX 10.9, *)
enum MKAnnotationViewDragState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case starting
  case dragging
  case canceling
  case ending
}
@available(OSX 10.9, *)
class MKAnnotationView : NSView {
  init(annotation annotation: MKAnnotation?, reuseIdentifier reuseIdentifier: String?)
  var reuseIdentifier: String? { get }
  var annotation: MKAnnotation?
  var image: NSImage?
  var centerOffset: CGPoint
  var calloutOffset: CGPoint
  var leftCalloutOffset: CGPoint
  var rightCalloutOffset: CGPoint
  var isEnabled: Bool
  var isHighlighted: Bool
  var isSelected: Bool
  func setSelected(_ selected: Bool, animated animated: Bool)
  var canShowCallout: Bool
  var leftCalloutAccessoryView: NSView?
  var rightCalloutAccessoryView: NSView?
  @available(OSX 10.11, *)
  var detailCalloutAccessoryView: NSView?
  @available(OSX 10.9, *)
  var isDraggable: Bool
  @available(OSX 10.9, *)
  var dragState: MKAnnotationViewDragState
  @available(OSX 10.9, *)
  func setDragState(_ newDragState: MKAnnotationViewDragState, animated animated: Bool)
}
