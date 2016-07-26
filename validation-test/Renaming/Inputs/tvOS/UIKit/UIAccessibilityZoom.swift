
@available(tvOS 5.0, *)
enum UIAccessibilityZoomType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case insertionPoint
}
@available(tvOS 5.0, *)
func UIAccessibilityZoomFocusChanged(_ type: UIAccessibilityZoomType, _ frame: CGRect, _ view: UIView)
@available(tvOS 5.0, *)
func UIAccessibilityRegisterGestureConflictWithZoom()
