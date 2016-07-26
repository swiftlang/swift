
@available(iOS 8.0, *)
class AVPlayerViewController : UIViewController {
  var showsPlaybackControls: Bool
  var videoGravity: String
  var isReadyForDisplay: Bool { get }
  var videoBounds: CGRect { get }
  var contentOverlayView: UIView? { get }
  @available(iOS 9.0, *)
  var allowsPictureInPicturePlayback: Bool
  @available(iOS 9.0, *)
  weak var delegate: @sil_weak AVPlayerViewControllerDelegate?
}
protocol AVPlayerViewControllerDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  optional func playerViewControllerWillStartPicture(inPicture playerViewController: AVPlayerViewController)
  @available(iOS 8.0, *)
  optional func playerViewControllerDidStartPicture(inPicture playerViewController: AVPlayerViewController)
  @available(iOS 8.0, *)
  optional func playerViewController(_ playerViewController: AVPlayerViewController, failedToStartPictureInPictureWithError error: NSError)
  @available(iOS 8.0, *)
  optional func playerViewControllerWillStopPicture(inPicture playerViewController: AVPlayerViewController)
  @available(iOS 8.0, *)
  optional func playerViewControllerDidStopPicture(inPicture playerViewController: AVPlayerViewController)
  @available(iOS 8.0, *)
  @discardableResult
  optional func playerViewControllerShouldAutomaticallyDismissAtPicture(inPictureStart playerViewController: AVPlayerViewController) -> Bool
  @available(iOS 8.0, *)
  optional func playerViewController(_ playerViewController: AVPlayerViewController, restoreUserInterfaceForPictureInPictureStopWithCompletionHandler completionHandler: (Bool) -> Void)
}
