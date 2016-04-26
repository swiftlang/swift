
@available(OSX 10.7, *)
class AVPlayerLayer : CALayer {
  /*not inherited*/ init(player player: AVPlayer?)
  var player: AVPlayer?
  var videoGravity: String
  var isReadyForDisplay: Bool { get }
  @available(OSX 10.9, *)
  var videoRect: CGRect { get }
  @available(OSX 10.11, *)
  var pixelBufferAttributes: [String : AnyObject]?
}
