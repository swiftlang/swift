
@available(iOS 8.0, *)
class AVAudioMixerNode : AVAudioNode, AVAudioMixing {
  var outputVolume: Float
  var nextAvailableInputBus: AVAudioNodeBus { get }
}
