
@available(iOS 9.3, *)
enum SKCloudServiceAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case denied
  case restricted
  case authorized
}
@available(iOS 9.3, *)
struct SKCloudServiceCapability : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var musicCatalogPlayback: SKCloudServiceCapability { get }
  static var addToCloudMusicLibrary: SKCloudServiceCapability { get }
}
@available(iOS 9.3, *)
class SKCloudServiceController : NSObject {
  @discardableResult
  class func authorizationStatus() -> SKCloudServiceAuthorizationStatus
  class func requestAuthorization(_ handler: (SKCloudServiceAuthorizationStatus) -> Void)
  func requestStorefrontIdentifier(completionHandler completionHandler: (String?, NSError?) -> Void)
  func requestCapabilities(completionHandler completionHandler: (SKCloudServiceCapability, NSError?) -> Void)
}
@available(iOS 9.3, *)
let SKStorefrontIdentifierDidChangeNotification: String
@available(iOS 9.3, *)
let SKCloudServiceCapabilitiesDidChangeNotification: String
