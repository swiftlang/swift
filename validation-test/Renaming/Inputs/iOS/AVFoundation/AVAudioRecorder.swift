
@available(iOS 3.0, *)
class AVAudioRecorder : NSObject {
  init(url url: NSURL, settings settings: [String : AnyObject]) throws
  @discardableResult
  func prepareToRecord() -> Bool
  @discardableResult
  func record() -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  func record(atTime time: NSTimeInterval) -> Bool
  @discardableResult
  func record(forDuration duration: NSTimeInterval) -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  func record(atTime time: NSTimeInterval, forDuration duration: NSTimeInterval) -> Bool
  func pause()
  func stop()
  @discardableResult
  func deleteRecording() -> Bool
  var isRecording: Bool { get }
  var url: NSURL { get }
  var settings: [String : AnyObject] { get }
  unowned(unsafe) var delegate: @sil_unmanaged AVAudioRecorderDelegate?
  var currentTime: NSTimeInterval { get }
  @available(iOS 6.0, *)
  var deviceCurrentTime: NSTimeInterval { get }
  var isMeteringEnabled: Bool
  func updateMeters()
  @discardableResult
  func peakPower(forChannel channelNumber: Int) -> Float
  @discardableResult
  func averagePower(forChannel channelNumber: Int) -> Float
  @available(iOS 7.0, *)
  var channelAssignments: [NSNumber]?
}
protocol AVAudioRecorderDelegate : NSObjectProtocol {
  @available(iOS 3.0, *)
  optional func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder, successfully flag: Bool)
  @available(iOS 3.0, *)
  optional func audioRecorderEncodeErrorDidOccur(_ recorder: AVAudioRecorder, error error: NSError?)
  @available(iOS, introduced: 2.2, deprecated: 8.0)
  optional func audioRecorderBeginInterruption(_ recorder: AVAudioRecorder)
  @available(iOS, introduced: 6.0, deprecated: 8.0)
  optional func audioRecorderEndInterruption(_ recorder: AVAudioRecorder, withOptions flags: Int)
}
