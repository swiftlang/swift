
@available(iOS 8.0, *)
enum AVAudioUnitEQFilterType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case parametric
  case lowPass
  case highPass
  case resonantLowPass
  case resonantHighPass
  case bandPass
  case bandStop
  case lowShelf
  case highShelf
  case resonantLowShelf
  case resonantHighShelf
}
@available(iOS 8.0, *)
class AVAudioUnitEQFilterParameters : NSObject {
  var filterType: AVAudioUnitEQFilterType
  var frequency: Float
  var bandwidth: Float
  var gain: Float
  var bypass: Bool
}
@available(iOS 8.0, *)
class AVAudioUnitEQ : AVAudioUnitEffect {
  init(numberOfBands numberOfBands: Int)
  var bands: [AVAudioUnitEQFilterParameters] { get }
  var globalGain: Float
}
