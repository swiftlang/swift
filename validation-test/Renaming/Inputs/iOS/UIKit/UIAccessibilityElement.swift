
@available(iOS 3.0, *)
class UIAccessibilityElement : NSObject, UIAccessibilityIdentification {
  init(accessibilityContainer container: AnyObject)
  unowned(unsafe) var accessibilityContainer: @sil_unmanaged AnyObject?
}
