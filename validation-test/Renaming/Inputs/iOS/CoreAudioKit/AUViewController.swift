
@available(iOS 9.0, *)
class AUViewController : UIViewController, NSExtensionRequestHandling {
}
extension AUAudioUnit {
  func requestViewController(completionHandler completionHandler: (UIViewController?) -> Void)
}
