
@available(OSX 10.10, *)
class AVAudioUnit : AVAudioNode {
  @available(OSX 10.11, *)
  class func instantiate(with audioComponentDescription: AudioComponentDescription, options options: AudioComponentInstantiationOptions = [], completionHandler completionHandler: (AVAudioUnit?, NSError?) -> Void)
  func loadPreset(at url: NSURL) throws
  var audioComponentDescription: AudioComponentDescription { get }
  var audioUnit: AudioUnit { get }
  @available(OSX 10.11, *)
  var auAudioUnit: AUAudioUnit { get }
  var name: String { get }
  var manufacturerName: String { get }
  var version: Int { get }
}
