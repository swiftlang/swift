
@available(OSX 10.10, *)
class AVAudioMixerNode : AVAudioNode, AVAudioMixing {
  var outputVolume: Float
  var nextAvailableInputBus: AVAudioNodeBus { get }
}
