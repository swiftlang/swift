
@available(tvOS 9.0, *)
class AVPlayerViewController : UIViewController {
  var player: AVPlayer?
  var showsPlaybackControls: Bool
  var requiresLinearPlayback: Bool
  var videoGravity: String
  var isReadyForDisplay: Bool { get }
  var contentOverlayView: UIView? { get }
  weak var delegate: @sil_weak AVPlayerViewControllerDelegate?
}
extension AVPlayerViewController {
  var allowedSubtitleOptionLanguages: [String]?
  var requiresFullSubtitles: Bool
}
protocol AVPlayerViewControllerDelegate : NSObjectProtocol {
  @available(tvOS 9.0, *)
  optional func playerViewController(_ playerViewController: AVPlayerViewController, willPresent interstitial: AVInterstitialTimeRange)
  @available(tvOS 9.0, *)
  optional func playerViewController(_ playerViewController: AVPlayerViewController, didPresent interstitial: AVInterstitialTimeRange)
  @available(tvOS 9.0, *)
  optional func playerViewController(_ playerViewController: AVPlayerViewController, willResumePlaybackAfterUserNavigatedFrom oldTime: CMTime, to targetTime: CMTime)
  @available(tvOS 9.0, *)
  optional func playerViewController(_ playerViewController: AVPlayerViewController, didSelect mediaSelectionOption: AVMediaSelectionOption?, in mediaSelectionGroup: AVMediaSelectionGroup)
}
