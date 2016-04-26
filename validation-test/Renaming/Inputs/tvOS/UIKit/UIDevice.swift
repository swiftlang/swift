
enum UIUserInterfaceIdiom : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unspecified
  @available(tvOS 3.2, *)
  case phone
  @available(tvOS 3.2, *)
  case pad
  @available(tvOS 9.0, *)
  case TV
  @available(tvOS 9.0, *)
  case carPlay
}
@available(tvOS 2.0, *)
class UIDevice : NSObject {
  @discardableResult
  class func current() -> UIDevice
  var name: String { get }
  var model: String { get }
  var localizedModel: String { get }
  var systemName: String { get }
  var systemVersion: String { get }
  @available(tvOS 6.0, *)
  var identifierForVendor: NSUUID? { get }
  @available(tvOS 3.0, *)
  var isProximityMonitoringEnabled: Bool
  @available(tvOS 3.0, *)
  var proximityState: Bool { get }
  @available(tvOS 4.0, *)
  var isMultitaskingSupported: Bool { get }
  @available(tvOS 3.2, *)
  var userInterfaceIdiom: UIUserInterfaceIdiom { get }
  @available(tvOS 4.2, *)
  func playInputClick()
}
protocol UIInputViewAudioFeedback : NSObjectProtocol {
  optional var enableInputClicksWhenVisible: Bool { get }
}
@discardableResult
func UI_USER_INTERFACE_IDIOM() -> UIUserInterfaceIdiom
@available(tvOS 3.0, *)
let UIDeviceProximityStateDidChangeNotification: String
