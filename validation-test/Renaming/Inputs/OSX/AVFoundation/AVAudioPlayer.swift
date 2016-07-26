
@available(OSX 10.7, *)
class AVAudioPlayer : NSObject {
  init(contentsOf url: NSURL) throws
  init(data data: NSData) throws
  @available(OSX 10.9, *)
  init(contentsOf url: NSURL, fileTypeHint utiString: String?) throws
  @available(OSX 10.9, *)
  init(data data: NSData, fileTypeHint utiString: String?) throws
  @discardableResult
  func prepareToPlay() -> Bool
  @discardableResult
  func play() -> Bool
  @available(OSX 10.7, *)
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
  @available(OSX 10.7, *)
  var pan: Float
  var volume: Float
  @available(OSX 10.8, *)
  var enableRate: Bool
  @available(OSX 10.8, *)
  var rate: Float
  var currentTime: NSTimeInterval
  @available(OSX 10.7, *)
  var deviceCurrentTime: NSTimeInterval { get }
  var numberOfLoops: Int
  @available(OSX 10.7, *)
  var settings: [String : AnyObject] { get }
  var isMeteringEnabled: Bool
  func updateMeters()
  @discardableResult
  func peakPower(forChannel channelNumber: Int) -> Float
  @discardableResult
  func averagePower(forChannel channelNumber: Int) -> Float
}
protocol AVAudioPlayerDelegate : NSObjectProtocol {
  @available(OSX 10.7, *)
  optional func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool)
  @available(OSX 10.7, *)
  optional func audioPlayerDecodeErrorDidOccur(_ player: AVAudioPlayer, error error: NSError?)
}
