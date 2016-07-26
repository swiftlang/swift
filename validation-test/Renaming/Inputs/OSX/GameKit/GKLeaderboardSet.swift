
@available(OSX 10.10, *)
class GKLeaderboardSet : NSObject, NSCoding, NSSecureCoding {
  var title: String { get }
  var groupIdentifier: String? { get }
  var identifier: String?
  @available(OSX 10.10, *)
  class func loadLeaderboardSets(completionHandler completionHandler: (([GKLeaderboardSet]?, NSError?) -> Void)? = nil)
  @available(OSX 10.10, *)
  func loadLeaderboards(completionHandler completionHandler: (([GKLeaderboard]?, NSError?) -> Void)? = nil)
}
extension GKLeaderboardSet {
  func loadImage(completionHandler completionHandler: ((NSImage?, NSError?) -> Void)? = nil)
}
