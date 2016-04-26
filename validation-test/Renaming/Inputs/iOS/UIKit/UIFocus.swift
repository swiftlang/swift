
@available(iOS 9.0, *)
struct UIFocusHeading : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var up: UIFocusHeading { get }
  static var down: UIFocusHeading { get }
  static var left: UIFocusHeading { get }
  static var right: UIFocusHeading { get }
  static var next: UIFocusHeading { get }
  static var previous: UIFocusHeading { get }
}
@available(iOS 9.0, *)
class UIFocusUpdateContext : NSObject {
  weak var previouslyFocusedView: @sil_weak UIView? { get }
  weak var nextFocusedView: @sil_weak UIView? { get }
  var focusHeading: UIFocusHeading { get }
}
@available(iOS 9.0, *)
protocol UIFocusEnvironment : NSObjectProtocol {
  weak var preferredFocusedView: @sil_weak UIView? { get }
  func setNeedsFocusUpdate()
  func updateFocusIfNeeded()
  @discardableResult
  func shouldUpdateFocus(in context: UIFocusUpdateContext) -> Bool
  func didUpdateFocus(in context: UIFocusUpdateContext, with coordinator: UIFocusAnimationCoordinator)
}
@available(iOS 9.0, *)
class UIFocusGuide : UILayoutGuide {
  var isEnabled: Bool
  weak var preferredFocusedView: @sil_weak UIView?
}
