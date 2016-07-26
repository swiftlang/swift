
@available(tvOS 8.0, *)
class AVAudioUnitSampler : AVAudioUnitMIDIInstrument {
  func loadSoundBankInstrument(at bankURL: NSURL, program program: UInt8, bankMSB bankMSB: UInt8, bankLSB bankLSB: UInt8) throws
  func loadInstrument(at instrumentURL: NSURL) throws
  func loadAudioFiles(at audioFiles: [NSURL]) throws
  var stereoPan: Float
  var masterGain: Float
  var globalTuning: Float
}
