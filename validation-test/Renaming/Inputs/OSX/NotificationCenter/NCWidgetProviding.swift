
@available(OSX 10.10, *)
enum NCUpdateResult : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case newData
  case noData
  case failed
}
protocol NCWidgetProviding : NSExtensionRequestHandling {
  @available(OSX 10.10, *)
  optional func widgetPerformUpdate(completionHandler completionHandler: ((NCUpdateResult) -> Void)!)
  @discardableResult
  optional func widgetMarginInsets(forProposedMarginInsets defaultMarginInset: NSEdgeInsets) -> NSEdgeInsets
  optional var widgetAllowsEditing: Bool { get }
  optional func widgetDidBeginEditing()
  optional func widgetDidEndEditing()
}
extension NSViewController {
  @available(OSX 10.10, *)
  func present(inWidget viewController: NSViewController!)
}
