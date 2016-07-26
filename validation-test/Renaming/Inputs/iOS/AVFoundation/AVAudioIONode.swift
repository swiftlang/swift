
@available(iOS 8.0, *)
class AVAudioIONode : AVAudioNode {
  var presentationLatency: NSTimeInterval { get }
  var audioUnit: AudioUnit? { get }
}
@available(iOS 8.0, *)
class AVAudioInputNode : AVAudioIONode, AVAudioMixing {
}
@available(iOS 8.0, *)
class AVAudioOutputNode : AVAudioIONode {
}
