
@available(tvOS 4.0, *)
class AVPlayerLayer : CALayer {
  /*not inherited*/ init(player player: AVPlayer?)
  var player: AVPlayer?
  var videoGravity: String
  var isReadyForDisplay: Bool { get }
  @available(tvOS 7.0, *)
  var videoRect: CGRect { get }
  @available(tvOS 9.0, *)
  var pixelBufferAttributes: [String : AnyObject]?
}
