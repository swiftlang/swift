
extension NSObject {
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityAttributeNames() -> [AnyObject]
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityAttributeNames() -> [AnyObject]
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityAttributeValue(_ attribute: String) -> AnyObject?
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityAttributeValue(_ attribute: String) -> AnyObject?
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityIsAttributeSettable(_ attribute: String) -> Bool
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityIsAttributeSettable(_ attribute: String) -> Bool
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  class func accessibilitySetValue(_ value: AnyObject?, forAttribute attribute: String)
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  func accessibilitySetValue(_ value: AnyObject?, forAttribute attribute: String)
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityParameterizedAttributeNames() -> [AnyObject]
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityParameterizedAttributeNames() -> [AnyObject]
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityAttributeValue(_ attribute: String, forParameter parameter: AnyObject?) -> AnyObject?
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityAttributeValue(_ attribute: String, forParameter parameter: AnyObject?) -> AnyObject?
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityActionNames() -> [AnyObject]
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityActionNames() -> [AnyObject]
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilityActionDescription(_ action: String) -> String?
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilityActionDescription(_ action: String) -> String?
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  class func accessibilityPerformAction(_ action: String)
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  func accessibilityPerformAction(_ action: String)
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use isAccessibilityElement instead")
  @discardableResult
  class func accessibilityIsIgnored() -> Bool
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use isAccessibilityElement instead")
  @discardableResult
  func accessibilityIsIgnored() -> Bool
  @discardableResult
  class func accessibilityHitTest(_ point: NSPoint) -> AnyObject?
  @discardableResult
  func accessibilityHitTest(_ point: NSPoint) -> AnyObject?
  var accessibilityFocusedUIElement: AnyObject? { get }
  @discardableResult
  class func accessibilityIndex(ofChild child: AnyObject) -> Int
  @discardableResult
  func accessibilityIndex(ofChild child: AnyObject) -> Int
  @discardableResult
  class func accessibilityArrayAttributeCount(_ attribute: String) -> Int
  @discardableResult
  func accessibilityArrayAttributeCount(_ attribute: String) -> Int
  @discardableResult
  class func accessibilityArrayAttributeValues(_ attribute: String, index index: Int, maxCount maxCount: Int) -> [AnyObject]
  @discardableResult
  func accessibilityArrayAttributeValues(_ attribute: String, index index: Int, maxCount maxCount: Int) -> [AnyObject]
  @available(OSX 10.9, *)
  var accessibilityNotifiesWhenDestroyed: Bool { get }
  class func accessibilityFocusedUIElement() -> AnyObject?
  class func accessibilityNotifiesWhenDestroyed() -> Bool
}
extension NSWorkspace {
  @available(OSX 10.10, *)
  var accessibilityDisplayShouldIncreaseContrast: Bool { get }
  @available(OSX 10.10, *)
  var accessibilityDisplayShouldDifferentiateWithoutColor: Bool { get }
  @available(OSX 10.10, *)
  var accessibilityDisplayShouldReduceTransparency: Bool { get }
}
@available(OSX 10.10, *)
let NSWorkspaceAccessibilityDisplayOptionsDidChangeNotification: String
extension NSObject {
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  class func accessibilitySetOverrideValue(_ value: AnyObject?, forAttribute attribute: String) -> Bool
  @available(OSX, introduced: 10.1, deprecated: 10.10, message: "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h)")
  @discardableResult
  func accessibilitySetOverrideValue(_ value: AnyObject?, forAttribute attribute: String) -> Bool
}
@available(OSX 10.10, *)
@discardableResult
func NSAccessibilityFrameInView(_ parentView: NSView, _ frame: NSRect) -> NSRect
@available(OSX 10.10, *)
@discardableResult
func NSAccessibilityPointInView(_ parentView: NSView, _ point: NSPoint) -> NSPoint
@discardableResult
func NSAccessibilitySetMayContainProtectedContent(_ flag: Bool) -> Bool
@discardableResult
func NSAccessibilityRoleDescription(_ role: String, _ subrole: String?) -> String?
@discardableResult
func NSAccessibilityRoleDescriptionForUIElement(_ element: AnyObject) -> String?
@discardableResult
func NSAccessibilityActionDescription(_ action: String) -> String?
@available(OSX, introduced: 10.1, deprecated: 10.11, message: "Exceptions are no longer appropriate for indicating errors in accessibility API. Unexpected values should be handled through appropriate type checking.")
func NSAccessibilityRaiseBadArgumentException(_ element: AnyObject!, _ attribute: String!, _ value: AnyObject!)
@discardableResult
func NSAccessibilityUnignoredAncestor(_ element: AnyObject) -> AnyObject?
@discardableResult
func NSAccessibilityUnignoredDescendant(_ element: AnyObject) -> AnyObject?
@discardableResult
func NSAccessibilityUnignoredChildren(_ originalChildren: [AnyObject]) -> [AnyObject]
@discardableResult
func NSAccessibilityUnignoredChildrenForOnlyChild(_ originalChild: AnyObject) -> [AnyObject]
func NSAccessibilityPostNotification(_ element: AnyObject, _ notification: String)
