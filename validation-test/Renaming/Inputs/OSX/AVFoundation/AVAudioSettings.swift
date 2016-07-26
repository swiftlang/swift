
let AVFormatIDKey: String
let AVSampleRateKey: String
let AVNumberOfChannelsKey: String
let AVLinearPCMBitDepthKey: String
let AVLinearPCMIsBigEndianKey: String
let AVLinearPCMIsFloatKey: String
@available(OSX 10.7, *)
let AVLinearPCMIsNonInterleaved: String
let AVEncoderAudioQualityKey: String
@available(OSX 10.9, *)
let AVEncoderAudioQualityForVBRKey: String
let AVEncoderBitRateKey: String
@available(OSX 10.7, *)
let AVEncoderBitRatePerChannelKey: String
@available(OSX 10.9, *)
let AVEncoderBitRateStrategyKey: String
let AVEncoderBitDepthHintKey: String
@available(OSX 10.9, *)
let AVSampleRateConverterAlgorithmKey: String
let AVSampleRateConverterAudioQualityKey: String
@available(OSX 10.7, *)
let AVChannelLayoutKey: String
@available(OSX 10.9, *)
let AVAudioBitRateStrategy_Constant: String
@available(OSX 10.9, *)
let AVAudioBitRateStrategy_LongTermAverage: String
@available(OSX 10.9, *)
let AVAudioBitRateStrategy_VariableConstrained: String
@available(OSX 10.9, *)
let AVAudioBitRateStrategy_Variable: String
@available(OSX 10.9, *)
let AVSampleRateConverterAlgorithm_Normal: String
@available(OSX 10.9, *)
let AVSampleRateConverterAlgorithm_Mastering: String
enum AVAudioQuality : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case min
  case low
  case medium
  case high
  case max
}
