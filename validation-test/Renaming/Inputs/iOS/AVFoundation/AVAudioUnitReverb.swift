
@available(iOS 8.0, *)
enum AVAudioUnitReverbPreset : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case smallRoom
  case mediumRoom
  case largeRoom
  case mediumHall
  case largeHall
  case plate
  case mediumChamber
  case largeChamber
  case cathedral
  case largeRoom2
  case mediumHall2
  case mediumHall3
  case largeHall2
}
@available(iOS 8.0, *)
class AVAudioUnitReverb : AVAudioUnitEffect {
  func loadFactoryPreset(_ preset: AVAudioUnitReverbPreset)
  var wetDryMix: Float
}
