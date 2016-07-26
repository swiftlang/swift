
@available(iOS 7.0, *)
let AVOutputSettingsPreset640x480: String
@available(iOS 7.0, *)
let AVOutputSettingsPreset960x540: String
@available(iOS 7.0, *)
let AVOutputSettingsPreset1280x720: String
@available(iOS 7.0, *)
let AVOutputSettingsPreset1920x1080: String
@available(iOS 9.0, *)
let AVOutputSettingsPreset3840x2160: String
@available(iOS 7.0, *)
class AVOutputSettingsAssistant : NSObject {
  @available(iOS 7.0, *)
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
  @available(iOS 7.0, *)
  var sourceVideoMinFrameDuration: CMTime
}
