
@available(watchOS 2.0, *)
enum WKHapticType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notification
  case directionUp
  case directionDown
  case success
  case failure
  case retry
  case start
  case stop
  case click
}
@available(watchOS 2.1, *)
enum WKInterfaceLayoutDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case leftToRight
  case rightToLeft
}
@available(watchOS 2.1, *)
enum WKInterfaceSemanticContentAttribute : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unspecified
  case playback
  case spatial
  case forceLeftToRight
  case forceRightToLeft
}
class WKInterfaceDevice : NSObject {
  @discardableResult
  class func current() -> WKInterfaceDevice
  var screenBounds: CGRect { get }
  var screenScale: CGFloat { get }
  var preferredContentSizeCategory: String { get }
  @available(watchOS 2.1, *)
  var layoutDirection: WKInterfaceLayoutDirection { get }
  @available(watchOS 2.1, *)
  @discardableResult
  class func interfaceLayoutDirection(for semanticContentAttribute: WKInterfaceSemanticContentAttribute) -> WKInterfaceLayoutDirection
  @available(watchOS 2.0, *)
  var systemVersion: String { get }
  @available(watchOS 2.0, *)
  var name: String { get }
  @available(watchOS 2.0, *)
  var model: String { get }
  @available(watchOS 2.0, *)
  var localizedModel: String { get }
  @available(watchOS 2.0, *)
  var systemName: String { get }
  @available(watchOS 2.0, *)
  func play(_ type: WKHapticType)
}
