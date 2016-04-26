
struct AUGenericViewDisplayFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var viewTitleDisplayFlag: AUGenericViewDisplayFlags { get }
  static var viewPropertiesDisplayFlag: AUGenericViewDisplayFlags { get }
  static var viewParametersDisplayFlag: AUGenericViewDisplayFlags { get }
}
class AUGenericView : NSView, AUCustomViewPersistentData {
  var audioUnit: AudioUnit { get }
  var showsExpertParameters: Bool
  init(audioUnit au: AudioUnit)
  init(audioUnit inAudioUnit: AudioUnit, displayFlags inFlags: AUGenericViewDisplayFlags)
}
