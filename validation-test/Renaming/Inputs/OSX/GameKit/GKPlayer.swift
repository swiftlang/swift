
@available(OSX 10.8, *)
class GKPlayer : NSObject {
  class func loadPlayers(forIdentifiers identifiers: [String], withCompletionHandler completionHandler: (([GKPlayer]?, NSError?) -> Void)? = nil)
  var playerID: String? { get }
  @available(OSX 10.8, *)
  var displayName: String? { get }
  var alias: String? { get }
  @available(OSX 10.11, *)
  @discardableResult
  class func anonymousGuestPlayer(withIdentifier guestIdentifier: String) -> Self
  @available(OSX 10.11, *)
  var guestIdentifier: String? { get }
}
extension GKPlayer {
  @available(OSX 10.8, *)
  func loadPhoto(forSize size: GKPhotoSize, withCompletionHandler completionHandler: ((NSImage?, NSError?) -> Void)? = nil)
}
var GKPhotoSizeSmall: Int { get }
var GKPhotoSizeNormal: Int { get }
typealias GKPhotoSize = Int
let GKPlayerDidChangeNotificationName: String
extension GKPlayer {
  @available(OSX, introduced: 10.8, deprecated: 10.10, message: "use -[GKLocalPlayer loadFriendPlayers...]")
  var isFriend: Bool { get }
}
