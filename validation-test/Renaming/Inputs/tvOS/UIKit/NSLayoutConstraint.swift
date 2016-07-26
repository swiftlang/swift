
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
  @available(tvOS 8.0, *)
  case firstBaseline
  @available(tvOS 8.0, *)
  case leftMargin
  @available(tvOS 8.0, *)
  case rightMargin
  @available(tvOS 8.0, *)
  case topMargin
  @available(tvOS 8.0, *)
  case bottomMargin
  @available(tvOS 8.0, *)
  case leadingMargin
  @available(tvOS 8.0, *)
  case trailingMargin
  @available(tvOS 8.0, *)
  case centerXWithinMargins
  @available(tvOS 8.0, *)
  case centerYWithinMargins
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
  @available(tvOS 8.0, *)
  static var alignAllFirstBaseline: NSLayoutFormatOptions { get }
  static var alignmentMask: NSLayoutFormatOptions { get }
  static var directionLeftToRight: NSLayoutFormatOptions { get }
  static var directionRightToLeft: NSLayoutFormatOptions { get }
  static var directionMask: NSLayoutFormatOptions { get }
}
typealias UILayoutPriority = Float
@available(tvOS 6.0, *)
let UILayoutPriorityRequired: UILayoutPriority
@available(tvOS 6.0, *)
let UILayoutPriorityDefaultHigh: UILayoutPriority
@available(tvOS 6.0, *)
let UILayoutPriorityDefaultLow: UILayoutPriority
@available(tvOS 6.0, *)
let UILayoutPriorityFittingSizeLevel: UILayoutPriority
@available(tvOS 6.0, *)
class NSLayoutConstraint : NSObject {
  @discardableResult
  class func constraints(withVisualFormat format: String, options opts: NSLayoutFormatOptions = [], metrics metrics: [String : AnyObject]?, views views: [String : AnyObject]) -> [NSLayoutConstraint]
  convenience init(item view1: AnyObject, attribute attr1: NSLayoutAttribute, relatedBy relation: NSLayoutRelation, toItem view2: AnyObject?, attribute attr2: NSLayoutAttribute, multiplier multiplier: CGFloat, constant c: CGFloat)
  var priority: UILayoutPriority
  var shouldBeArchived: Bool
  unowned(unsafe) var firstItem: @sil_unmanaged AnyObject { get }
  var firstAttribute: NSLayoutAttribute { get }
  var relation: NSLayoutRelation { get }
  unowned(unsafe) var secondItem: @sil_unmanaged AnyObject? { get }
  var secondAttribute: NSLayoutAttribute { get }
  var multiplier: CGFloat { get }
  var constant: CGFloat
  @available(tvOS 8.0, *)
  var isActive: Bool
  @available(tvOS 8.0, *)
  class func activate(_ constraints: [NSLayoutConstraint])
  @available(tvOS 8.0, *)
  class func deactivate(_ constraints: [NSLayoutConstraint])
}
extension NSLayoutConstraint {
  @available(tvOS 7.0, *)
  var identifier: String?
}
protocol UILayoutSupport : NSObjectProtocol {
  var length: CGFloat { get }
  @available(tvOS 9.0, *)
  var topAnchor: NSLayoutYAxisAnchor { get }
  @available(tvOS 9.0, *)
  var bottomAnchor: NSLayoutYAxisAnchor { get }
  @available(tvOS 9.0, *)
  var heightAnchor: NSLayoutDimension { get }
}
