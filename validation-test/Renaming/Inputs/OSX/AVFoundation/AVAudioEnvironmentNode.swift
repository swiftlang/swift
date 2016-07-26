
@available(OSX 10.10, *)
enum AVAudioEnvironmentDistanceAttenuationModel : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case exponential
  case inverse
  case linear
}
@available(OSX 10.10, *)
class AVAudioEnvironmentDistanceAttenuationParameters : NSObject {
  var distanceAttenuationModel: AVAudioEnvironmentDistanceAttenuationModel
  var referenceDistance: Float
  var maximumDistance: Float
  var rolloffFactor: Float
}
@available(OSX 10.10, *)
class AVAudioEnvironmentReverbParameters : NSObject {
  var enable: Bool
  var level: Float
  var filterParameters: AVAudioUnitEQFilterParameters { get }
  func loadFactoryReverbPreset(_ preset: AVAudioUnitReverbPreset)
}
@available(OSX 10.10, *)
class AVAudioEnvironmentNode : AVAudioNode, AVAudioMixing {
  var outputVolume: Float
  var nextAvailableInputBus: AVAudioNodeBus { get }
  var listenerPosition: AVAudio3DPoint
  var listenerVectorOrientation: AVAudio3DVectorOrientation
  var listenerAngularOrientation: AVAudio3DAngularOrientation
  var distanceAttenuationParameters: AVAudioEnvironmentDistanceAttenuationParameters { get }
  var reverbParameters: AVAudioEnvironmentReverbParameters { get }
  var applicableRenderingAlgorithms: [NSNumber] { get }
}
