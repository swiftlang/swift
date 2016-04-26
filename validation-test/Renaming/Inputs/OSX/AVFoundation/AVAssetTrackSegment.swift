
@available(OSX 10.7, *)
class AVAssetTrackSegment : NSObject {
  var timeMapping: CMTimeMapping { get }
  var isEmpty: Bool { get }
}
