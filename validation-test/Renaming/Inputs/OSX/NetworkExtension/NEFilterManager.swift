
@available(OSX 10.10, *)
enum NEFilterManagerError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case configurationInvalid
  case configurationDisabled
  case configurationStale
  case configurationCannotBeRemoved
}
@available(OSX 10.10, *)
let NEFilterErrorDomain: String
@available(OSX 10.10, *)
let NEFilterConfigurationDidChangeNotification: String
@available(OSX 10.10, *)
class NEFilterManager : NSObject {
  @available(OSX 10.10, *)
  @discardableResult
  class func shared() -> NEFilterManager
  @available(OSX 10.10, *)
  func loadFromPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.10, *)
  func removeFromPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.10, *)
  func saveToPreferences(completionHandler completionHandler: (NSError?) -> Void)
  @available(OSX 10.10, *)
  var localizedDescription: String?
  @available(OSX 10.11, *)
  var providerConfiguration: NEFilterProviderConfiguration?
  @available(OSX 10.10, *)
  var isEnabled: Bool
}
