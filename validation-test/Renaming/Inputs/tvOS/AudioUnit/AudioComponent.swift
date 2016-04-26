
struct AudioComponentFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  @available(tvOS 5.0, *)
  static var unsearchable: AudioComponentFlags { get }
  @available(tvOS 6.0, *)
  static var sandboxSafe: AudioComponentFlags { get }
  @available(tvOS 9.0, *)
  static var isv3AudioUnit: AudioComponentFlags { get }
  @available(tvOS 9.0, *)
  static var requiresAsyncInstantiation: AudioComponentFlags { get }
  @available(tvOS 9.0, *)
  static var canLoadInProcess: AudioComponentFlags { get }
}
struct AudioComponentInstantiationOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  @available(tvOS 9.0, *)
  static var loadOutOfProcess: AudioComponentInstantiationOptions { get }
}
struct AudioComponentDescription {
  var componentType: OSType
  var componentSubType: OSType
  var componentManufacturer: OSType
  var componentFlags: UInt32
  var componentFlagsMask: UInt32
  init()
  init(componentType componentType: OSType, componentSubType componentSubType: OSType, componentManufacturer componentManufacturer: OSType, componentFlags componentFlags: UInt32, componentFlagsMask componentFlagsMask: UInt32)
}
typealias AudioComponent = OpaquePointer
typealias AudioComponentInstance = OpaquePointer
typealias AudioComponentMethod = OpaquePointer
struct AudioComponentPlugInInterface {
  var Open: @convention(c) (UnsafeMutablePointer<Void>, AudioComponentInstance) -> OSStatus
  var Close: @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
  var Lookup: @convention(c) (Int16) -> AudioComponentMethod?
  var reserved: UnsafeMutablePointer<Void>?
}
typealias AudioComponentFactoryFunction = @convention(c) (UnsafePointer<AudioComponentDescription>) -> UnsafeMutablePointer<AudioComponentPlugInInterface>?
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentFindNext(_ inComponent: AudioComponent?, _ inDesc: UnsafePointer<AudioComponentDescription>) -> AudioComponent?
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentCount(_ inDesc: UnsafePointer<AudioComponentDescription>) -> UInt32
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentCopyName(_ inComponent: AudioComponent, _ outName: UnsafeMutablePointer<Unmanaged<CFString>?>) -> OSStatus
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentGetDescription(_ inComponent: AudioComponent, _ outDesc: UnsafeMutablePointer<AudioComponentDescription>) -> OSStatus
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentGetVersion(_ inComponent: AudioComponent, _ outVersion: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentInstanceNew(_ inComponent: AudioComponent, _ outInstance: UnsafeMutablePointer<AudioComponentInstance?>) -> OSStatus
@available(tvOS 9.0, *)
func AudioComponentInstantiate(_ inComponent: AudioComponent, _ inOptions: AudioComponentInstantiationOptions, _ inCompletionHandler: (AudioComponentInstance?, OSStatus) -> Void)
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentInstanceDispose(_ inInstance: AudioComponentInstance) -> OSStatus
@available(tvOS 2.0, *)
@discardableResult
func AudioComponentInstanceGetComponent(_ inInstance: AudioComponentInstance) -> AudioComponent
@available(tvOS 3.0, *)
@discardableResult
func AudioComponentInstanceCanDo(_ inInstance: AudioComponentInstance, _ inSelectorID: Int16) -> Bool
@available(tvOS 5.0, *)
@discardableResult
func AudioComponentRegister(_ inDesc: UnsafePointer<AudioComponentDescription>, _ inName: CFString, _ inVersion: UInt32, _ inFactory: AudioComponentFactoryFunction) -> AudioComponent
enum AudioComponentValidationResult : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case unknown
  case passed
  case failed
  case timedOut
  case unauthorizedError_Open
  case unauthorizedError_Init
}
var kAudioComponentConfigurationInfo_ValidationResult: String { get }
var kAudioComponentValidationParameter_TimeOut: String { get }
var kAudioComponentValidationParameter_ForceValidation: String { get }
