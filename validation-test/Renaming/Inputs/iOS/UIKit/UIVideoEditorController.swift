
@available(iOS 3.1, *)
class UIVideoEditorController : UINavigationController {
  @available(iOS 3.1, *)
  @discardableResult
  class func canEditVideo(atPath videoPath: String) -> Bool
  var videoPath: String
  var videoMaximumDuration: NSTimeInterval
  var videoQuality: UIImagePickerControllerQualityType
}
protocol UIVideoEditorControllerDelegate : NSObjectProtocol {
  @available(iOS 3.1, *)
  optional func videoEditorController(_ editor: UIVideoEditorController, didSaveEditedVideoToPath editedVideoPath: String)
  @available(iOS 3.1, *)
  optional func videoEditorController(_ editor: UIVideoEditorController, didFailWithError error: NSError)
  @available(iOS 3.1, *)
  optional func videoEditorControllerDidCancel(_ editor: UIVideoEditorController)
}
