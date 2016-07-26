
@available(tvOS 8.0, *)
class AVAudioUnitDelay : AVAudioUnitEffect {
  var delayTime: NSTimeInterval
  var feedback: Float
  var lowPassCutoff: Float
  var wetDryMix: Float
}
