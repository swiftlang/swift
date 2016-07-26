
enum NSLayoutRelation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case lessThanOrEqual
  case equal
  case greaterThanOrEqual
}
enum NSLayoutAttribute : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case left
  case right
  case top
  case bottom
  case leading
  case trailing
  case width
  case height
  case centerX
  case centerY
  case baseline
  static var lastBaseline: NSLayoutAttribute { get }
  @available(OSX 10.11, *)
  case firstBaseline
  case notAnAttribute
}
struct NSLayoutFormatOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var alignAllLeft: NSLayoutFormatOptions { get }
  static var alignAllRight: NSLayoutFormatOptions { get }
  static var alignAllTop: NSLayoutFormatOptions { get }
  static var alignAllBottom: NSLayoutFormatOptions { get }
  static var alignAllLeading: NSLayoutFormatOptions { get }
  static var alignAllTrailing: NSLayoutFormatOptions { get }
  static var alignAllCenterX: NSLayoutFormatOptions { get }
  static var alignAllCenterY: NSLayoutFormatOptions { get }
  static var alignAllBaseline: NSLayoutFormatOptions { get }
  static var alignAllLastBaseline: NSLayoutFormatOptions { get }
  @available(OSX 10.11, *)
  static var alignAllFirstBaseline: NSLayoutFormatOptions { get }
  static var alignmentMask: NSLayoutFormatOptions { get }
  static var directionLeftToRight: NSLayoutFormatOptions { get }
  static var directionRightToLeft: NSLayoutFormatOptions { get }
  static var directionMask: NSLayoutFormatOptions { get }
}
enum NSLayoutConstraintOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case horizontal
  case vertical
}
@available(OSX 10.7, *)
typealias NSLayoutPriority = Float
@available(OSX 10.7, *)
let NSLayoutPriorityRequired: NSLayoutPriority
@available(OSX 10.7, *)
let NSLayoutPriorityDefaultHigh: NSLayoutPriority
@available(OSX 10.7, *)
let NSLayoutPriorityDragThatCanResizeWindow: NSLayoutPriority
@available(OSX 10.7, *)
let NSLayoutPriorityWindowSizeStayPut: NSLayoutPriority
@available(OSX 10.7, *)
let NSLayoutPriorityDragThatCannotResizeWindow: NSLayoutPriority
@available(OSX 10.7, *)
let NSLayoutPriorityDefaultLow: NSLayoutPriority
@available(OSX 10.7, *)
let NSLayoutPriorityFittingSizeCompression: NSLayoutPriority
@available(OSX 10.7, *)
class NSLayoutConstraint : NSObject, NSAnimatablePropertyContainer {
  @discardableResult
  class func constraints(withVisualFormat format: String, options opts: NSLayoutFormatOptions = [], metrics metrics: [String : NSNumber]?, views views: [String : AnyObject]) -> [NSLayoutConstraint]
  convenience init(item view1: AnyObject, attribute attr1: NSLayoutAttribute, relatedBy relation: NSLayoutRelation, toItem view2: AnyObject?, attribute attr2: NSLayoutAttribute, multiplier multiplier: CGFloat, constant c: CGFloat)
  var priority: NSLayoutPriority
  var shouldBeArchived: Bool
  unowned(unsafe) var firstItem: @sil_unmanaged AnyObject { get }
  var firstAttribute: NSLayoutAttribute { get }
  var relation: NSLayoutRelation { get }
  unowned(unsafe) var secondItem: @sil_unmanaged AnyObject? { get }
  var secondAttribute: NSLayoutAttribute { get }
  var multiplier: CGFloat { get }
  var constant: CGFloat
  @available(OSX 10.10, *)
  var isActive: Bool
  @available(OSX 10.10, *)
  class func activate(_ constraints: [NSLayoutConstraint])
  @available(OSX 10.10, *)
  class func deactivate(_ constraints: [NSLayoutConstraint])
}
extension NSLayoutConstraint {
  var identifier: String?
}
extension NSView {
  @available(OSX 10.11, *)
  var leadingAnchor: NSLayoutXAxisAnchor { get }
  @available(OSX 10.11, *)
  var trailingAnchor: NSLayoutXAxisAnchor { get }
  @available(OSX 10.11, *)
  var leftAnchor: NSLayoutXAxisAnchor { get }
  @available(OSX 10.11, *)
  var rightAnchor: NSLayoutXAxisAnchor { get }
  @available(OSX 10.11, *)
  var topAnchor: NSLayoutYAxisAnchor { get }
  @available(OSX 10.11, *)
  var bottomAnchor: NSLayoutYAxisAnchor { get }
  @available(OSX 10.11, *)
  var widthAnchor: NSLayoutDimension { get }
  @available(OSX 10.11, *)
  var heightAnchor: NSLayoutDimension { get }
  @available(OSX 10.11, *)
  var centerXAnchor: NSLayoutXAxisAnchor { get }
  @available(OSX 10.11, *)
  var centerYAnchor: NSLayoutYAxisAnchor { get }
  @available(OSX 10.11, *)
  var firstBaselineAnchor: NSLayoutYAxisAnchor { get }
  @available(OSX 10.11, *)
  var lastBaselineAnchor: NSLayoutYAxisAnchor { get }
  @available(OSX 10.7, *)
  var constraints: [NSLayoutConstraint] { get }
  @available(OSX 10.7, *)
  func addConstraint(_ constraint: NSLayoutConstraint)
  @available(OSX 10.7, *)
  func addConstraints(_ constraints: [NSLayoutConstraint])
  @available(OSX 10.7, *)
  func removeConstraint(_ constraint: NSLayoutConstraint)
  @available(OSX 10.7, *)
  func removeConstraints(_ constraints: [NSLayoutConstraint])
}
extension NSWindow {
  @available(OSX 10.7, *)
  func updateConstraintsIfNeeded()
  @available(OSX 10.7, *)
  func layoutIfNeeded()
}
extension NSView {
  @available(OSX 10.7, *)
  func updateConstraintsForSubtreeIfNeeded()
  @available(OSX 10.7, *)
  func updateConstraints()
  @available(OSX 10.7, *)
  var needsUpdateConstraints: Bool
  @available(OSX 10.7, *)
  func layoutSubtreeIfNeeded()
  @available(OSX 10.7, *)
  func layout()
  @available(OSX 10.7, *)
  var needsLayout: Bool
}
extension NSView {
  @available(OSX 10.7, *)
  var translatesAutoresizingMaskIntoConstraints: Bool
  @available(OSX 10.7, *)
  @discardableResult
  class func requiresConstraintBasedLayout() -> Bool
}
extension NSView {
  @available(OSX 10.7, *)
  @discardableResult
  func alignmentRect(forFrame frame: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func frame(forAlignmentRect alignmentRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  var alignmentRectInsets: NSEdgeInsets { get }
  @available(OSX 10.11, *)
  var firstBaselineOffsetFromTop: CGFloat { get }
  @available(OSX 10.11, *)
  var lastBaselineOffsetFromBottom: CGFloat { get }
  @available(OSX 10.7, *)
  var baselineOffsetFromBottom: CGFloat { get }
  @available(OSX 10.7, *)
  var intrinsicContentSize: NSSize { get }
  @available(OSX 10.7, *)
  func invalidateIntrinsicContentSize()
  @available(OSX 10.7, *)
  @discardableResult
  func contentHuggingPriority(for orientation: NSLayoutConstraintOrientation) -> NSLayoutPriority
  @available(OSX 10.7, *)
  func setContentHuggingPriority(_ priority: NSLayoutPriority, for orientation: NSLayoutConstraintOrientation)
  @available(OSX 10.7, *)
  @discardableResult
  func contentCompressionResistancePriority(for orientation: NSLayoutConstraintOrientation) -> NSLayoutPriority
  @available(OSX 10.7, *)
  func setContentCompressionResistancePriority(_ priority: NSLayoutPriority, for orientation: NSLayoutConstraintOrientation)
}
let NSViewNoInstrinsicMetric: CGFloat
@available(OSX 10.11, *)
let NSViewNoIntrinsicMetric: CGFloat
extension NSControl {
  @available(OSX 10.7, *)
  func invalidateIntrinsicContentSize(for cell: NSCell)
}
extension NSWindow {
  @discardableResult
  func anchorAttribute(for orientation: NSLayoutConstraintOrientation) -> NSLayoutAttribute
  func setAnchorAttribute(_ attr: NSLayoutAttribute, for orientation: NSLayoutConstraintOrientation)
}
extension NSView {
  @available(OSX 10.7, *)
  var fittingSize: NSSize { get }
}
extension NSView {
  @available(OSX 10.7, *)
  @discardableResult
  func constraintsAffectingLayout(for orientation: NSLayoutConstraintOrientation) -> [NSLayoutConstraint]
  @available(OSX 10.7, *)
  var hasAmbiguousLayout: Bool { get }
  @available(OSX 10.7, *)
  func exerciseAmbiguityInLayout()
}
extension NSWindow {
  @available(OSX 10.7, *)
  func visualizeConstraints(_ constraints: [NSLayoutConstraint])
}
