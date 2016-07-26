
typealias AVAudioNodeTapBlock = (AVAudioPCMBuffer, AVAudioTime) -> Void
@available(iOS 8.0, *)
class AVAudioNode : NSObject {
  func reset()
  @discardableResult
  func inputFormat(forBus bus: AVAudioNodeBus) -> AVAudioFormat
  @discardableResult
  func outputFormat(forBus bus: AVAudioNodeBus) -> AVAudioFormat
  @discardableResult
  func name(forInputBus bus: AVAudioNodeBus) -> String
  @discardableResult
  func name(forOutputBus bus: AVAudioNodeBus) -> String
  func installTap(onBus bus: AVAudioNodeBus, bufferSize bufferSize: AVAudioFrameCount, format format: AVAudioFormat?, block tapBlock: AVAudioNodeTapBlock)
  func removeTap(onBus bus: AVAudioNodeBus)
  var engine: AVAudioEngine? { get }
  var numberOfInputs: Int { get }
  var numberOfOutputs: Int { get }
  var lastRenderTime: AVAudioTime? { get }
}
