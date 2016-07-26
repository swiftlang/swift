
@available(OSX 10.10, *)
enum AVAudioUnitDistortionPreset : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case drumsBitBrush
  case drumsBufferBeats
  case drumsLoFi
  case multiBrokenSpeaker
  case multiCellphoneConcert
  case multiDecimated1
  case multiDecimated2
  case multiDecimated3
  case multiDecimated4
  case multiDistortedFunk
  case multiDistortedCubed
  case multiDistortedSquared
  case multiEcho1
  case multiEcho2
  case multiEchoTight1
  case multiEchoTight2
  case multiEverythingIsBroken
  case speechAlienChatter
  case speechCosmicInterference
  case speechGoldenPi
  case speechRadioTower
  case speechWaves
}
@available(OSX 10.10, *)
class AVAudioUnitDistortion : AVAudioUnitEffect {
  func loadFactoryPreset(_ preset: AVAudioUnitDistortionPreset)
  var preGain: Float
  var wetDryMix: Float
}
