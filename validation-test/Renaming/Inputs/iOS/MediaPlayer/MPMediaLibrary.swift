
@available(iOS 9.3, *)
enum MPMediaLibraryAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case denied
  case restricted
  case authorized
}
@available(iOS 3.0, *)
class MPMediaLibrary : NSObject, NSSecureCoding {
  @discardableResult
  class func defaultMediaLibrary() -> MPMediaLibrary
  var lastModifiedDate: NSDate { get }
  func beginGeneratingLibraryChangeNotifications()
  func endGeneratingLibraryChangeNotifications()
  @available(iOS 9.3, *)
  @discardableResult
  class func authorizationStatus() -> MPMediaLibraryAuthorizationStatus
  @available(iOS 9.3, *)
  class func requestAuthorization(_ handler: (MPMediaLibraryAuthorizationStatus) -> Void)
  @available(iOS 9.3, *)
  func addItem(withProductID productID: String, completionHandler completionHandler: (([MPMediaEntity], NSError?) -> Void)? = nil)
  @available(iOS 9.3, *)
  func getPlaylistWith(_ uuid: NSUUID, creationMetadata creationMetadata: MPMediaPlaylistCreationMetadata?, completionHandler completionHandler: (MPMediaPlaylist?, NSError?) -> Void)
}
let MPMediaLibraryDidChangeNotification: String
