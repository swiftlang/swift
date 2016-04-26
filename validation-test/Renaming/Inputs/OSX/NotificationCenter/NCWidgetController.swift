
@available(OSX 10.10, *)
class NCWidgetController : NSObject {
  @discardableResult
  class func widgetController() -> Self!
  @available(*, deprecated, message: "Use widgetController instead.")
  @discardableResult
  class func defaultWidgetController() -> NCWidgetController!
  func setHasContent(_ flag: Bool, forWidgetWithBundleIdentifier bundleID: String!)
}
