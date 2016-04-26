
class RPPreviewViewController : UIViewController {
  weak var previewControllerDelegate: @sil_weak RPPreviewViewControllerDelegate?
}
@available(iOS 9.0, *)
protocol RPPreviewViewControllerDelegate : NSObjectProtocol {
  optional func previewControllerDidFinish(_ previewController: RPPreviewViewController)
  optional func previewController(_ previewController: RPPreviewViewController, didFinishWithActivityTypes activityTypes: Set<String>)
}
