
@available(tvOS 4.0, *)
class AVPlayerItemTrack : NSObject {
  var assetTrack: AVAssetTrack { get }
  var isEnabled: Bool
  @available(tvOS 7.0, *)
  var currentVideoFrameRate: Float { get }
}
