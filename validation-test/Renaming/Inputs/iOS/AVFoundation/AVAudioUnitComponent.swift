
@available(iOS 9.0, *)
let AVAudioUnitTypeOutput: String
@available(iOS 9.0, *)
let AVAudioUnitTypeMusicDevice: String
@available(iOS 9.0, *)
let AVAudioUnitTypeMusicEffect: String
@available(iOS 9.0, *)
let AVAudioUnitTypeFormatConverter: String
@available(iOS 9.0, *)
let AVAudioUnitTypeEffect: String
@available(iOS 9.0, *)
let AVAudioUnitTypeMixer: String
@available(iOS 9.0, *)
let AVAudioUnitTypePanner: String
@available(iOS 9.0, *)
let AVAudioUnitTypeGenerator: String
@available(iOS 9.0, *)
let AVAudioUnitTypeOfflineEffect: String
@available(iOS 9.0, *)
let AVAudioUnitTypeMIDIProcessor: String
@available(iOS 9.0, *)
let AVAudioUnitManufacturerNameApple: String
@available(iOS 9.0, *)
class AVAudioUnitComponent : NSObject {
  var name: String { get }
  var typeName: String { get }
  var localizedTypeName: String { get }
  var manufacturerName: String { get }
  var version: Int { get }
  var versionString: String { get }
  var isSandboxSafe: Bool { get }
  var hasMIDIInput: Bool { get }
  var hasMIDIOutput: Bool { get }
  var audioComponent: AudioComponent { get }
  var allTagNames: [String] { get }
  var audioComponentDescription: AudioComponentDescription { get }
}
@available(iOS 9.0, *)
let AVAudioUnitComponentTagsDidChangeNotification: String
@available(iOS 9.0, *)
class AVAudioUnitComponentManager : NSObject {
  var tagNames: [String] { get }
  var standardLocalizedTagNames: [String] { get }
  @discardableResult
  class func shared() -> Self
  @discardableResult
  func components(matching predicate: NSPredicate) -> [AVAudioUnitComponent]
  @discardableResult
  func components(passingTest testHandler: (AVAudioUnitComponent, UnsafeMutablePointer<ObjCBool>) -> Bool) -> [AVAudioUnitComponent]
  @discardableResult
  func components(matching desc: AudioComponentDescription) -> [AVAudioUnitComponent]
}
