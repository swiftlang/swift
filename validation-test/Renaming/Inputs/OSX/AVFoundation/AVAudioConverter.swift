
enum AVAudioConverterPrimeMethod : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case pre
  case normal
  case none
}
struct AVAudioConverterPrimeInfo {
  var leadingFrames: AVAudioFrameCount
  var trailingFrames: AVAudioFrameCount
  init()
  init(leadingFrames leadingFrames: AVAudioFrameCount, trailingFrames trailingFrames: AVAudioFrameCount)
}
@available(OSX 10.11, *)
enum AVAudioConverterInputStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case haveData
  case noDataNow
  case endOfStream
}
@available(OSX 10.11, *)
enum AVAudioConverterOutputStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case haveData
  case inputRanDry
  case endOfStream
  case error
}
typealias AVAudioConverterInputBlock = (AVAudioPacketCount, UnsafeMutablePointer<AVAudioConverterInputStatus>) -> AVAudioBuffer?
@available(OSX 10.11, *)
class AVAudioConverter : NSObject {
  init(from fromFormat: AVAudioFormat, to toFormat: AVAudioFormat)
  func reset()
  var inputFormat: AVAudioFormat { get }
  var outputFormat: AVAudioFormat { get }
  var channelMap: [NSNumber]
  var magicCookie: NSData?
  var downmix: Bool
  var dither: Bool
  var sampleRateConverterQuality: Int
  var sampleRateConverterAlgorithm: String
  var primeMethod: AVAudioConverterPrimeMethod
  var primeInfo: AVAudioConverterPrimeInfo
  func convert(to outputBuffer: AVAudioPCMBuffer, from inputBuffer: AVAudioPCMBuffer) throws
  @discardableResult
  func convert(to outputBuffer: AVAudioBuffer, error outError: NSErrorPointer, withInputFrom inputBlock: AVAudioConverterInputBlock) -> AVAudioConverterOutputStatus
}
extension AVAudioConverter {
  var bitRate: Int
  var bitRateStrategy: String?
  var maximumOutputPacketSize: Int { get }
  var availableEncodeBitRates: [NSNumber]? { get }
  var applicableEncodeBitRates: [NSNumber]? { get }
  var availableEncodeSampleRates: [NSNumber]? { get }
  var applicableEncodeSampleRates: [NSNumber]? { get }
  var availableEncodeChannelLayoutTags: [NSNumber]? { get }
}
