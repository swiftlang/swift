
struct UIControlEvents : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var touchDown: UIControlEvents { get }
  static var touchDownRepeat: UIControlEvents { get }
  static var touchDragInside: UIControlEvents { get }
  static var touchDragOutside: UIControlEvents { get }
  static var touchDragEnter: UIControlEvents { get }
  static var touchDragExit: UIControlEvents { get }
  static var touchUpInside: UIControlEvents { get }
  static var touchUpOutside: UIControlEvents { get }
  static var touchCancel: UIControlEvents { get }
  static var valueChanged: UIControlEvents { get }
  @available(iOS 9.0, *)
  static var primaryActionTriggered: UIControlEvents { get }
  static var editingDidBegin: UIControlEvents { get }
  static var editingChanged: UIControlEvents { get }
  static var editingDidEnd: UIControlEvents { get }
  static var editingDidEndOnExit: UIControlEvents { get }
  static var allTouchEvents: UIControlEvents { get }
  static var allEditingEvents: UIControlEvents { get }
  static var applicationReserved: UIControlEvents { get }
  static var systemReserved: UIControlEvents { get }
  static var allEvents: UIControlEvents { get }
}
enum UIControlContentVerticalAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case center
  case top
  case bottom
  case fill
}
enum UIControlContentHorizontalAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case center
  case left
  case right
  case fill
}
struct UIControlState : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var highlighted: UIControlState { get }
  static var disabled: UIControlState { get }
  static var selected: UIControlState { get }
  @available(iOS 9.0, *)
  static var focused: UIControlState { get }
  static var application: UIControlState { get }
  static var reserved: UIControlState { get }
}
@available(iOS 2.0, *)
class UIControl : UIView {
  var isEnabled: Bool
  var isSelected: Bool
  var isHighlighted: Bool
  var contentVerticalAlignment: UIControlContentVerticalAlignment
  var contentHorizontalAlignment: UIControlContentHorizontalAlignment
  var state: UIControlState { get }
  var isTracking: Bool { get }
  var isTouchInside: Bool { get }
  @discardableResult
  func beginTracking(with touch: UITouch, with event: UIEvent?) -> Bool
  @discardableResult
  func continueTracking(with touch: UITouch, with event: UIEvent?) -> Bool
  func endTracking(with touch: UITouch?, with event: UIEvent?)
  func cancelTracking(with event: UIEvent?)
  func addTarget(_ target: AnyObject?, action action: Selector, for controlEvents: UIControlEvents)
  func removeTarget(_ target: AnyObject?, action action: Selector?, for controlEvents: UIControlEvents)
  @discardableResult
  func allTargets() -> Set<NSObject>
  @discardableResult
  func allControlEvents() -> UIControlEvents
  @discardableResult
  func actions(forTarget target: AnyObject?, forControlEvent controlEvent: UIControlEvents) -> [String]?
  func sendAction(_ action: Selector, to target: AnyObject?, for event: UIEvent?)
  func sendActions(for controlEvents: UIControlEvents)
}
