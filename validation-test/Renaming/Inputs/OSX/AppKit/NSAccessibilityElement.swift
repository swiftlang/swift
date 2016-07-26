
@available(OSX 10.10, *)
class NSAccessibilityElement : NSObject, NSAccessibility {
  @discardableResult
  class func accessibilityElement(withRole role: String, frame frame: NSRect, label label: String?, parent parent: AnyObject?) -> AnyObject
  func accessibilityAddChildElement(_ childElement: NSAccessibilityElement)
  @discardableResult
  func accessibilityFrameInParentSpace() -> NSRect
  func setAccessibilityFrameInParentSpace(_ accessibilityFrameInParentSpace: NSRect)
}
