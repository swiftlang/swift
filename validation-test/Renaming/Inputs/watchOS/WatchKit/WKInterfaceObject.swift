
@available(watchOS 2.0, *)
enum WKInterfaceObjectHorizontalAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case left
  case center
  case right
}
@available(watchOS 2.0, *)
enum WKInterfaceObjectVerticalAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case top
  case center
  case bottom
}
@available(watchOS 2.0, *)
class WKInterfaceObject : NSObject {
  func setHidden(_ hidden: Bool)
  func setAlpha(_ alpha: CGFloat)
  @available(watchOS 2.1, *)
  func setSemanticContentAttribute(_ semanticContentAttribute: WKInterfaceSemanticContentAttribute)
  @available(watchOS 2.0, *)
  func setHorizontalAlignment(_ horizontalAlignment: WKInterfaceObjectHorizontalAlignment)
  @available(watchOS 2.0, *)
  func setVerticalAlignment(_ verticalAlignment: WKInterfaceObjectVerticalAlignment)
  func setWidth(_ width: CGFloat)
  func setHeight(_ height: CGFloat)
  @available(watchOS 2.0, *)
  func setRelativeWidth(_ width: CGFloat, withAdjustment adjustment: CGFloat)
  @available(watchOS 2.0, *)
  func setRelativeHeight(_ height: CGFloat, withAdjustment adjustment: CGFloat)
  @available(watchOS 2.0, *)
  func sizeToFitWidth()
  @available(watchOS 2.0, *)
  func sizeToFitHeight()
  var interfaceProperty: String { get }
}
extension WKInterfaceObject {
  @available(watchOS 2.0, *)
  func setAccessibilityIdentifier(_ accessibilityIdentifier: String?)
  func setAccessibilityLabel(_ accessibilityLabel: String?)
  func setAccessibilityHint(_ accessibilityHint: String?)
  func setAccessibilityValue(_ accessibilityValue: String?)
  func setIsAccessibilityElement(_ isAccessibilityElement: Bool)
  func setAccessibilityTraits(_ accessibilityTraits: UIAccessibilityTraits)
  func setAccessibilityImageRegions(_ accessibilityImageRegions: [WKAccessibilityImageRegion])
}
@available(watchOS 2.0, *)
class WKAccessibilityImageRegion : NSObject {
  var frame: CGRect
  var label: String
}
