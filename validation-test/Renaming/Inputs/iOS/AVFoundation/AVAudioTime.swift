
@available(iOS 8.0, *)
class AVAudioTime : NSObject {
  init(audioTimeStamp ts: UnsafePointer<AudioTimeStamp>, sampleRate sampleRate: Double)
  init(hostTime hostTime: UInt64)
  init(sampleTime sampleTime: AVAudioFramePosition, atRate sampleRate: Double)
  init(hostTime hostTime: UInt64, sampleTime sampleTime: AVAudioFramePosition, atRate sampleRate: Double)
  @discardableResult
  class func hostTime(forSeconds seconds: NSTimeInterval) -> UInt64
  @discardableResult
  class func seconds(forHostTime hostTime: UInt64) -> NSTimeInterval
  @discardableResult
  func extrapolateTime(fromAnchor anchorTime: AVAudioTime) -> AVAudioTime
  var isHostTimeValid: Bool { get }
  var hostTime: UInt64 { get }
  var isSampleTimeValid: Bool { get }
  var sampleTime: AVAudioFramePosition { get }
  var sampleRate: Double { get }
  var audioTimeStamp: AudioTimeStamp { get }
}
