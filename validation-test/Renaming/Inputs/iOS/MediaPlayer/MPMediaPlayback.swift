
protocol MPMediaPlayback {
  func prepareToPlay()
  var isPreparedToPlay: Bool { get }
  func play()
  func pause()
  func stop()
  var currentPlaybackTime: NSTimeInterval { get set }
  var currentPlaybackRate: Float { get set }
  func beginSeekingForward()
  func beginSeekingBackward()
  func endSeeking()
}
@available(iOS, introduced: 3.2, deprecated: 9.0)
let MPMediaPlaybackIsPreparedToPlayDidChangeNotification: String
