
@available(iOS 8.0, *)
enum NCUpdateResult : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case newData
  case noData
  case failed
}
protocol NCWidgetProviding : NSObjectProtocol {
  @available(iOS 8.0, *)
  optional func widgetPerformUpdate(completionHandler completionHandler: (NCUpdateResult) -> Void)
  @discardableResult
  optional func widgetMarginInsets(forProposedMarginInsets defaultMarginInsets: UIEdgeInsets) -> UIEdgeInsets
}
extension UIVibrancyEffect {
  @discardableResult
  class func notificationCenter() -> UIVibrancyEffect
}
