
@available(OSX 10.10, *)
class AVAudioUnitGenerator : AVAudioUnit, AVAudioMixing {
  init(audioComponentDescription audioComponentDescription: AudioComponentDescription)
  var bypass: Bool
}
