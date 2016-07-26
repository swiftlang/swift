
@available(OSX 10.11, *)
class AUViewController : NSViewController, NSExtensionRequestHandling {
}
extension AUAudioUnit {
  func requestViewController(completionHandler completionHandler: (NSViewController?) -> Void)
}
