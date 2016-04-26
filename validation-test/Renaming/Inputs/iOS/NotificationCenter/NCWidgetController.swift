
@available(iOS 8.0, *)
class NCWidgetController : NSObject {
  @discardableResult
  class func widgetController() -> Self
  func setHasContent(_ flag: Bool, forWidgetWithBundleIdentifier bundleID: String)
}
