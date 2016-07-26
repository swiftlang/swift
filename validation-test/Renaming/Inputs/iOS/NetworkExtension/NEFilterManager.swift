
@available(iOS 8.0, *)
enum NEFilterManagerError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case configurationInvalid
  case configurationDisabled
  case configurationStale
  case configurationCannotBeRemoved
}
@available(iOS 8.0, *)
let NEFilterErrorDomain: String
@available(iOS 8.0, *)
let NEFilterConfigurationDidChangeNotification: String
@available(iOS 8.0, *)
class NEFilterManager : NSObject {
  @available(iOS 8.0, *)
  @discardableResult
  class func shared() -> NEFilterManager
  @available(iOS 8.0, *)
  func loadFromPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 8.0, *)
  func removeFromPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 8.0, *)
  func saveToPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 8.0, *)
  var localizedDescription: String?
  @available(iOS 9.0, *)
  var providerConfiguration: NEFilterProviderConfiguration?
  @available(iOS 8.0, *)
  var isEnabled: Bool
}
