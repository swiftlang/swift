
@available(OSX 10.10, *)
class AVAudioIONode : AVAudioNode {
  var presentationLatency: NSTimeInterval { get }
  var audioUnit: AudioUnit? { get }
}
@available(OSX 10.10, *)
class AVAudioInputNode : AVAudioIONode, AVAudioMixing {
}
@available(OSX 10.10, *)
class AVAudioOutputNode : AVAudioIONode {
}
