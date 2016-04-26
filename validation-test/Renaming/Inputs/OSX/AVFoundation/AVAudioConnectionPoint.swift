
@available(OSX 10.11, *)
class AVAudioConnectionPoint : NSObject {
  init(node node: AVAudioNode, bus bus: AVAudioNodeBus)
  weak var node: @sil_weak AVAudioNode? { get }
  var bus: AVAudioNodeBus { get }
}
