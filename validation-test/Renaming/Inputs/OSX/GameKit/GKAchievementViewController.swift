
@available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use GKGameCenterViewController instead")
class GKAchievementViewController : GKGameCenterViewController {
}
extension GKAchievementViewController {
  unowned(unsafe) var achievementDelegate: @sil_unmanaged GKAchievementViewControllerDelegate!
}
@available(OSX, introduced: 10.8, deprecated: 10.10, message: "Use GKGameCenterViewController instead")
protocol GKAchievementViewControllerDelegate : NSObjectProtocol {
  func achievementViewControllerDidFinish(_ viewController: GKAchievementViewController!)
}
