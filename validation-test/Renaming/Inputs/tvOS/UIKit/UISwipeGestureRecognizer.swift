
struct UISwipeGestureRecognizerDirection : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var right: UISwipeGestureRecognizerDirection { get }
  static var left: UISwipeGestureRecognizerDirection { get }
  static var up: UISwipeGestureRecognizerDirection { get }
  static var down: UISwipeGestureRecognizerDirection { get }
}
@available(tvOS 3.2, *)
class UISwipeGestureRecognizer : UIGestureRecognizer {
  var direction: UISwipeGestureRecognizerDirection
}
