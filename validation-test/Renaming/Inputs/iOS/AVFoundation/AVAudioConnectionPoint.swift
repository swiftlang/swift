
@available(iOS 9.0, *)
class AVAudioConnectionPoint : NSObject {
  init(node node: AVAudioNode, bus bus: AVAudioNodeBus)
  weak var node: @sil_weak AVAudioNode? { get }
  var bus: AVAudioNodeBus { get }
}
