
@available(iOS 8.2, *)
class WKInterfaceObject : NSObject {
  func setHidden(_ hidden: Bool)
  func setAlpha(_ alpha: CGFloat)
  func setWidth(_ width: CGFloat)
  func setHeight(_ height: CGFloat)
  var interfaceProperty: String { get }
}
extension WKInterfaceObject {
  @available(iOS 9.0, *)
  func setAccessibilityIdentifier(_ accessibilityIdentifier: String?)
  func setAccessibilityImageRegions(_ accessibilityImageRegions: [WKAccessibilityImageRegion])
}
@available(iOS 8.2, *)
class WKAccessibilityImageRegion : NSObject {
  var frame: CGRect
  var label: String
}
