
@available(iOS 5.0, *)
enum UIAccessibilityZoomType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case insertionPoint
}
@available(iOS 5.0, *)
func UIAccessibilityZoomFocusChanged(_ type: UIAccessibilityZoomType, _ frame: CGRect, _ view: UIView)
@available(iOS 5.0, *)
func UIAccessibilityRegisterGestureConflictWithZoom()
