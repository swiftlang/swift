
@available(OSX 10.7, *)
class AVComposition : AVAsset, NSMutableCopying {
  @available(OSX 10.11, *)
  var urlAssetInitializationOptions: [String : AnyObject] { get }
}
extension AVComposition {
}
@available(OSX 10.7, *)
class AVMutableComposition : AVComposition {
  @available(OSX 10.11, *)
  convenience init(urlAssetInitializationOptions URLAssetInitializationOptions: [String : AnyObject]? = [:])
}
extension AVMutableComposition {
  func insertTimeRange(_ timeRange: CMTimeRange, of asset: AVAsset, at startTime: CMTime) throws
  func insertEmptyTimeRange(_ timeRange: CMTimeRange)
  func removeTimeRange(_ timeRange: CMTimeRange)
  func scaleTimeRange(_ timeRange: CMTimeRange, toDuration duration: CMTime)
}
extension AVMutableComposition {
  @discardableResult
  func addMutableTrack(withMediaType mediaType: String, preferredTrackID preferredTrackID: CMPersistentTrackID) -> AVMutableCompositionTrack
  func removeTrack(_ track: AVCompositionTrack)
  @discardableResult
  func mutableTrack(compatibleWith track: AVAssetTrack) -> AVMutableCompositionTrack?
}
extension AVMutableComposition {
}
