
let MKAnnotationCalloutInfoDidChangeNotification: String
@available(tvOS 9.2, *)
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
  @available(tvOS 9.0, *)
  var detailCalloutAccessoryView: UIView?
}
