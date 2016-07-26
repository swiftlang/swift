
@available(tvOS 9.0, *)
class UIFocusAnimationCoordinator : NSObject {
  func addCoordinatedAnimations(_ animations: (() -> Void)?, completion completion: (() -> Void)? = nil)
}
