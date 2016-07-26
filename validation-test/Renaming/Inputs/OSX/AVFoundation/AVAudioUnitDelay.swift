
@available(OSX 10.10, *)
class AVAudioUnitDelay : AVAudioUnitEffect {
  var delayTime: NSTimeInterval
  var feedback: Float
  var lowPassCutoff: Float
  var wetDryMix: Float
}
