
@available(iOS 4.1, *)
class GKPlayer : NSObject {
  class func loadPlayers(forIdentifiers identifiers: [String], withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  var playerID: String? { get }
  @available(iOS 6.0, *)
  var displayName: String? { get }
  var alias: String? { get }
  @available(iOS 9.0, *)
  @discardableResult
  class func anonymousGuestPlayer(withIdentifier guestIdentifier: String) -> Self
  @available(iOS 9.0, *)
  var guestIdentifier: String? { get }
}
extension GKPlayer {
  @available(iOS 5.0, *)
  func loadPhoto(forSize size: GKPhotoSize, withCompletionHandler completionHandler: ((UIImage?, NSError?) -> Void)? = nil)
}
var GKPhotoSizeSmall: Int { get }
var GKPhotoSizeNormal: Int { get }
typealias GKPhotoSize = Int
let GKPlayerDidChangeNotificationName: String
extension GKPlayer {
  @available(iOS, introduced: 4.1, deprecated: 8.0, message: "use -[GKLocalPlayer loadFriendPlayers...]")
  var isFriend: Bool { get }
}
