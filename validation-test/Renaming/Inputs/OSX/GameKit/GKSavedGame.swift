
@available(OSX 10.10, *)
class GKSavedGame : NSObject, NSCopying {
  @available(OSX 10.10, *)
  var name: String? { get }
  @available(OSX 10.10, *)
  var deviceName: String? { get }
  @available(OSX 10.10, *)
  var modificationDate: NSDate? { get }
  @available(OSX 10.10, *)
  func loadData(completionHandler handler: ((NSData?, NSError?) -> Void)? = nil)
}
extension GKLocalPlayer : GKSavedGameListener {
  @available(OSX 10.10, *)
  func fetchSavedGames(completionHandler handler: (([GKSavedGame]?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func saveGameData(_ data: NSData, withName name: String, completionHandler handler: ((GKSavedGame?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func deleteSavedGames(withName name: String, completionHandler handler: ((NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func resolveConflictingSavedGames(_ conflictingSavedGames: [GKSavedGame], with data: NSData, completionHandler handler: (([GKSavedGame]?, NSError?) -> Void)? = nil)
}
