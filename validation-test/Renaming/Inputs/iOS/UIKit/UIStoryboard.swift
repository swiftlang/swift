
@available(iOS 5.0, *)
class UIStoryboard : NSObject {
  /*not inherited*/ init(name name: String, bundle storyboardBundleOrNil: NSBundle?)
  @discardableResult
  func instantiateInitialViewController() -> UIViewController?
  @discardableResult
  func instantiateViewController(withIdentifier identifier: String) -> UIViewController
}
