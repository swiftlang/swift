
@available(iOS 9.1, *)
struct PHLivePhotoBadgeOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var overContent: PHLivePhotoBadgeOptions { get }
  static var liveOff: PHLivePhotoBadgeOptions { get }
}
@available(iOS 9.1, *)
enum PHLivePhotoViewPlaybackStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case undefined
  case full
  case hint
}
@available(iOS 9.1, *)
class PHLivePhotoView : UIView {
  @discardableResult
  class func livePhotoBadgeImage(_ badgeOptions: PHLivePhotoBadgeOptions = []) -> UIImage
  weak var delegate: @sil_weak PHLivePhotoViewDelegate?
  var livePhoto: PHLivePhoto?
  var isMuted: Bool
  var playbackGestureRecognizer: UIGestureRecognizer { get }
  func startPlayback(with playbackStyle: PHLivePhotoViewPlaybackStyle)
  func stopPlayback()
}
protocol PHLivePhotoViewDelegate : NSObjectProtocol {
  @available(iOS 9.1, *)
  optional func livePhotoView(_ livePhotoView: PHLivePhotoView, willBeginPlaybackWith playbackStyle: PHLivePhotoViewPlaybackStyle)
  @available(iOS 9.1, *)
  optional func livePhotoView(_ livePhotoView: PHLivePhotoView, didEndPlaybackWith playbackStyle: PHLivePhotoViewPlaybackStyle)
}
