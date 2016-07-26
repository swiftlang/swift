
@available(iOS 8.0, *)
class AVAudioUnit : AVAudioNode {
  @available(iOS 9.0, *)
  class func instantiate(with audioComponentDescription: AudioComponentDescription, options options: AudioComponentInstantiationOptions = [], completionHandler completionHandler: (AVAudioUnit?, NSError?) -> Void)
  func loadPreset(at url: NSURL) throws
  var audioComponentDescription: AudioComponentDescription { get }
  var audioUnit: AudioUnit { get }
  @available(iOS 9.0, *)
  var auAudioUnit: AUAudioUnit { get }
  var name: String { get }
  var manufacturerName: String { get }
  var version: Int { get }
}
