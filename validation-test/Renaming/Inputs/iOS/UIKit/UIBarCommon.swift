
enum UIBarMetrics : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case compact
  case defaultPrompt
  case compactPrompt
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "Use UIBarMetricsCompact instead")
  static var landscapePhone: UIBarMetrics { get }
  @available(iOS, introduced: 7.0, deprecated: 8.0, message: "Use UIBarMetricsCompactPrompt")
  static var landscapePhonePrompt: UIBarMetrics { get }
}
@available(iOS 7.0, *)
enum UIBarPosition : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case any
  case bottom
  case top
  case topAttached
}
protocol UIBarPositioning : NSObjectProtocol {
  @available(iOS 7.0, *)
  var barPosition: UIBarPosition { get }
}
protocol UIBarPositioningDelegate : NSObjectProtocol {
  @available(iOS 7.0, *)
  @discardableResult
  optional func position(forBar bar: UIBarPositioning) -> UIBarPosition
}
