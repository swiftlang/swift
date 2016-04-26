
@available(iOS 7.0, *)
enum UIAttachmentBehaviorType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case items
  case anchor
}
struct UIFloatRange {
  var minimum: CGFloat
  var maximum: CGFloat
  init()
  init(minimum minimum: CGFloat, maximum maximum: CGFloat)
}
@available(iOS 9.0, *)
let UIFloatRangeZero: UIFloatRange
@available(iOS 9.0, *)
let UIFloatRangeInfinite: UIFloatRange
@available(iOS 9.0, *)
@discardableResult
func UIFloatRangeIsInfinite(_ range: UIFloatRange) -> Bool
@available(iOS 9.0, *)
@discardableResult
func UIFloatRangeIsEqualToRange(_ range: UIFloatRange, _ otherRange: UIFloatRange) -> Bool
@discardableResult
func UIFloatRangeMake(_ minimum: CGFloat, _ maximum: CGFloat) -> UIFloatRange
@available(iOS 7.0, *)
class UIAttachmentBehavior : UIDynamicBehavior {
  convenience init(item item: UIDynamicItem, attachedToAnchor point: CGPoint)
  init(item item: UIDynamicItem, offsetFromCenter offset: UIOffset, attachedToAnchor point: CGPoint)
  convenience init(item item1: UIDynamicItem, attachedTo item2: UIDynamicItem)
  init(item item1: UIDynamicItem, offsetFromCenter offset1: UIOffset, attachedTo item2: UIDynamicItem, offsetFromCenter offset2: UIOffset)
  @available(iOS 9.0, *)
  @discardableResult
  class func slidingAttachment(with item1: UIDynamicItem, attachedTo item2: UIDynamicItem, attachmentAnchor point: CGPoint, axisOfTranslation axis: CGVector) -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func slidingAttachment(with item: UIDynamicItem, attachmentAnchor point: CGPoint, axisOfTranslation axis: CGVector) -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func limitAttachment(with item1: UIDynamicItem, offsetFromCenter offset1: UIOffset, attachedTo item2: UIDynamicItem, offsetFromCenter offset2: UIOffset) -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func fixedAttachment(with item1: UIDynamicItem, attachedTo item2: UIDynamicItem, attachmentAnchor point: CGPoint) -> Self
  @available(iOS 9.0, *)
  @discardableResult
  class func pinAttachment(with item1: UIDynamicItem, attachedTo item2: UIDynamicItem, attachmentAnchor point: CGPoint) -> Self
  var items: [UIDynamicItem] { get }
  var attachedBehaviorType: UIAttachmentBehaviorType { get }
  var anchorPoint: CGPoint
  var length: CGFloat
  var damping: CGFloat
  var frequency: CGFloat
  @available(iOS 9.0, *)
  var frictionTorque: CGFloat
  @available(iOS 9.0, *)
  var attachmentRange: UIFloatRange
}
