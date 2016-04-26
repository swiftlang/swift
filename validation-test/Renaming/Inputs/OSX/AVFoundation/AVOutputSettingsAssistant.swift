
@available(OSX 10.9, *)
let AVOutputSettingsPreset640x480: String
@available(OSX 10.9, *)
let AVOutputSettingsPreset960x540: String
@available(OSX 10.9, *)
let AVOutputSettingsPreset1280x720: String
@available(OSX 10.9, *)
let AVOutputSettingsPreset1920x1080: String
@available(OSX 10.10, *)
let AVOutputSettingsPreset3840x2160: String
@available(OSX 10.9, *)
class AVOutputSettingsAssistant : NSObject {
  @available(OSX 10.10, *)
  @discardableResult
  class func availableOutputSettingsPresets() -> [String]
  convenience init?(preset presetIdentifier: String)
  var audioSettings: [String : AnyObject]? { get }
  var videoSettings: [String : AnyObject]? { get }
  var outputFileType: String { get }
}
extension AVOutputSettingsAssistant {
  var sourceAudioFormat: CMAudioFormatDescription?
  var sourceVideoFormat: CMVideoFormatDescription?
  var sourceVideoAverageFrameDuration: CMTime
  @available(OSX 10.10, *)
  var sourceVideoMinFrameDuration: CMTime
}
