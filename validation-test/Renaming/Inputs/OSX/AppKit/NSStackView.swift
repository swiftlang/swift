
@available(OSX 10.9, *)
enum NSStackViewGravity : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case top
  static var leading: NSStackViewGravity { get }
  case center
  case bottom
  static var trailing: NSStackViewGravity { get }
}
@available(OSX 10.11, *)
enum NSStackViewDistribution : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case gravityAreas
  case fill
  case fillEqually
  case fillProportionally
  case equalSpacing
  case equalCentering
}
@available(OSX 10.9, *)
typealias NSStackViewVisibilityPriority = Float
@available(OSX 10.9, *)
let NSStackViewVisibilityPriorityMustHold: NSStackViewVisibilityPriority
@available(OSX 10.9, *)
let NSStackViewVisibilityPriorityDetachOnlyIfNecessary: NSStackViewVisibilityPriority
@available(OSX 10.9, *)
let NSStackViewVisibilityPriorityNotVisible: NSStackViewVisibilityPriority
@available(OSX 10.9, *)
let NSStackViewSpacingUseDefault: CGFloat
@available(OSX 10.9, *)
class NSStackView : NSView {
  convenience init(views views: [NSView])
  unowned(unsafe) var delegate: @sil_unmanaged NSStackViewDelegate?
  var orientation: NSUserInterfaceLayoutOrientation
  var alignment: NSLayoutAttribute
  var edgeInsets: NSEdgeInsets
  func addView(_ aView: NSView, in gravity: NSStackViewGravity)
  func insertView(_ aView: NSView, at index: Int, in gravity: NSStackViewGravity)
  func removeView(_ aView: NSView)
  @discardableResult
  func views(in gravity: NSStackViewGravity) -> [NSView]
  func setViews(_ views: [NSView], in gravity: NSStackViewGravity)
  var views: [NSView] { get }
  var detachedViews: [NSView] { get }
  @available(OSX 10.11, *)
  var detachesHiddenViews: Bool
  func setVisibilityPriority(_ priority: NSStackViewVisibilityPriority, for aView: NSView)
  @discardableResult
  func visibilityPriority(for aView: NSView) -> NSStackViewVisibilityPriority
  var spacing: CGFloat
  func setCustomSpacing(_ spacing: CGFloat, after aView: NSView)
  @discardableResult
  func customSpacing(after aView: NSView) -> CGFloat
  @available(OSX 10.11, *)
  var distribution: NSStackViewDistribution
  @discardableResult
  func clippingResistancePriority(for orientation: NSLayoutConstraintOrientation) -> NSLayoutPriority
  func setClippingResistancePriority(_ clippingResistancePriority: NSLayoutPriority, for orientation: NSLayoutConstraintOrientation)
  @discardableResult
  func huggingPriority(for orientation: NSLayoutConstraintOrientation) -> NSLayoutPriority
  func setHuggingPriority(_ huggingPriority: NSLayoutPriority, for orientation: NSLayoutConstraintOrientation)
}
extension NSStackView {
  @available(OSX 10.11, *)
  var arrangedSubviews: [NSView] { get }
  @available(OSX 10.11, *)
  func addArrangedSubview(_ view: NSView)
  @available(OSX 10.11, *)
  func insertArrangedSubview(_ view: NSView, at index: Int)
  @available(OSX 10.11, *)
  func removeArrangedSubview(_ view: NSView)
}
protocol NSStackViewDelegate : NSObjectProtocol {
  @available(OSX 10.9, *)
  optional func stackView(_ stackView: NSStackView, willDetach views: [NSView])
  @available(OSX 10.9, *)
  optional func stackView(_ stackView: NSStackView, didReattach views: [NSView])
}
extension NSStackView {
  @available(OSX, introduced: 10.9, deprecated: 10.11, message: "Set -distribution to NSStackViewDistributionEqualSpacing instead.")
  var hasEqualSpacing: Bool
}
