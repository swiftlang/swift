
@available(tvOS 8.0, *)
class AVAudioIONode : AVAudioNode {
  var presentationLatency: NSTimeInterval { get }
  var audioUnit: AudioUnit? { get }
}
@available(tvOS 8.0, *)
class AVAudioInputNode : AVAudioIONode, AVAudioMixing {
}
@available(tvOS 8.0, *)
class AVAudioOutputNode : AVAudioIONode {
}
