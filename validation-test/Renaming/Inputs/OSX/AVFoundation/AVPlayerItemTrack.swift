
@available(OSX 10.7, *)
class AVPlayerItemTrack : NSObject {
  var assetTrack: AVAssetTrack { get }
  var isEnabled: Bool
  @available(OSX 10.9, *)
  var currentVideoFrameRate: Float { get }
  @available(OSX 10.10, *)
  var videoFieldMode: String?
}
@available(OSX 10.10, *)
let AVPlayerItemTrackVideoFieldModeDeinterlaceFields: String
