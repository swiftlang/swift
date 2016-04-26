
let AVFormatIDKey: String
let AVSampleRateKey: String
let AVNumberOfChannelsKey: String
let AVLinearPCMBitDepthKey: String
let AVLinearPCMIsBigEndianKey: String
let AVLinearPCMIsFloatKey: String
@available(tvOS 4.0, *)
let AVLinearPCMIsNonInterleaved: String
let AVEncoderAudioQualityKey: String
@available(tvOS 7.0, *)
let AVEncoderAudioQualityForVBRKey: String
let AVEncoderBitRateKey: String
@available(tvOS 4.0, *)
let AVEncoderBitRatePerChannelKey: String
@available(tvOS 7.0, *)
let AVEncoderBitRateStrategyKey: String
let AVEncoderBitDepthHintKey: String
@available(tvOS 7.0, *)
let AVSampleRateConverterAlgorithmKey: String
let AVSampleRateConverterAudioQualityKey: String
@available(tvOS 4.0, *)
let AVChannelLayoutKey: String
@available(tvOS 7.0, *)
let AVAudioBitRateStrategy_Constant: String
@available(tvOS 7.0, *)
let AVAudioBitRateStrategy_LongTermAverage: String
@available(tvOS 7.0, *)
let AVAudioBitRateStrategy_VariableConstrained: String
@available(tvOS 7.0, *)
let AVAudioBitRateStrategy_Variable: String
@available(tvOS 7.0, *)
let AVSampleRateConverterAlgorithm_Normal: String
@available(tvOS 7.0, *)
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
