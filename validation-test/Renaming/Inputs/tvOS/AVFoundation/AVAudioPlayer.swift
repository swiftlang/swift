
@available(tvOS 2.2, *)
class AVAudioPlayer : NSObject {
  init(contentsOf url: NSURL) throws
  init(data data: NSData) throws
  @available(tvOS 7.0, *)
  init(contentsOf url: NSURL, fileTypeHint utiString: String?) throws
  @available(tvOS 7.0, *)
  init(data data: NSData, fileTypeHint utiString: String?) throws
  @discardableResult
  func prepareToPlay() -> Bool
  @discardableResult
  func play() -> Bool
  @available(tvOS 4.0, *)
  @discardableResult
  func play(atTime time: NSTimeInterval) -> Bool
  func pause()
  func stop()
  var isPlaying: Bool { get }
  var numberOfChannels: Int { get }
  var duration: NSTimeInterval { get }
  unowned(unsafe) var delegate: @sil_unmanaged AVAudioPlayerDelegate?
  var url: NSURL? { get }
  var data: NSData? { get }
  @available(tvOS 4.0, *)
  var pan: Float
  var volume: Float
  @available(tvOS 5.0, *)
  var enableRate: Bool
  @available(tvOS 5.0, *)
  var rate: Float
  var currentTime: NSTimeInterval
  @available(tvOS 4.0, *)
  var deviceCurrentTime: NSTimeInterval { get }
  var numberOfLoops: Int
  @available(tvOS 4.0, *)
  var settings: [String : AnyObject] { get }
  var isMeteringEnabled: Bool
  func updateMeters()
  @discardableResult
  func peakPower(forChannel channelNumber: Int) -> Float
  @discardableResult
  func averagePower(forChannel channelNumber: Int) -> Float
  @available(tvOS 7.0, *)
  var channelAssignments: [NSNumber]?
}
protocol AVAudioPlayerDelegate : NSObjectProtocol {
  @available(tvOS 2.2, *)
  optional func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool)
  @available(tvOS 2.2, *)
  optional func audioPlayerDecodeErrorDidOccur(_ player: AVAudioPlayer, error error: NSError?)
  @available(tvOS, introduced: 2.2, deprecated: 8.0)
  optional func audioPlayerBeginInterruption(_ player: AVAudioPlayer)
  @available(tvOS, introduced: 6.0, deprecated: 8.0)
  optional func audioPlayerEndInterruption(_ player: AVAudioPlayer, withOptions flags: Int)
}
