
struct AudioComponentFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  @available(OSX 10.7, *)
  static var unsearchable: AudioComponentFlags { get }
  @available(OSX 10.8, *)
  static var sandboxSafe: AudioComponentFlags { get }
  @available(OSX 10.11, *)
  static var isv3AudioUnit: AudioComponentFlags { get }
  @available(OSX 10.11, *)
  static var requiresAsyncInstantiation: AudioComponentFlags { get }
  @available(OSX 10.11, *)
  static var canLoadInProcess: AudioComponentFlags { get }
}
struct AudioComponentInstantiationOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  @available(OSX 10.11, *)
  static var loadOutOfProcess: AudioComponentInstantiationOptions { get }
  @available(OSX 10.11, *)
  static var loadInProcess: AudioComponentInstantiationOptions { get }
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
typealias AudioComponentInstance = UnsafeMutablePointer<ComponentInstanceRecord>
typealias AudioComponentMethod = OpaquePointer
struct AudioComponentPlugInInterface {
  var Open: @convention(c) (UnsafeMutablePointer<Void>, AudioComponentInstance) -> OSStatus
  var Close: @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
  var Lookup: @convention(c) (Int16) -> AudioComponentMethod?
  var reserved: UnsafeMutablePointer<Void>?
}
typealias AudioComponentFactoryFunction = @convention(c) (UnsafePointer<AudioComponentDescription>) -> UnsafeMutablePointer<AudioComponentPlugInInterface>?
@available(OSX 10.6, *)
@discardableResult
func AudioComponentFindNext(_ inComponent: AudioComponent?, _ inDesc: UnsafePointer<AudioComponentDescription>) -> AudioComponent?
@available(OSX 10.6, *)
@discardableResult
func AudioComponentCount(_ inDesc: UnsafePointer<AudioComponentDescription>) -> UInt32
@available(OSX 10.6, *)
@discardableResult
func AudioComponentCopyName(_ inComponent: AudioComponent, _ outName: UnsafeMutablePointer<Unmanaged<CFString>?>) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func AudioComponentGetDescription(_ inComponent: AudioComponent, _ outDesc: UnsafeMutablePointer<AudioComponentDescription>) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func AudioComponentGetVersion(_ inComponent: AudioComponent, _ outVersion: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.11, *)
@discardableResult
func AudioComponentGetIcon(_ comp: AudioComponent) -> NSImage?
@available(OSX 10.6, *)
@discardableResult
func AudioComponentInstanceNew(_ inComponent: AudioComponent, _ outInstance: UnsafeMutablePointer<AudioComponentInstance?>) -> OSStatus
@available(OSX 10.11, *)
func AudioComponentInstantiate(_ inComponent: AudioComponent, _ inOptions: AudioComponentInstantiationOptions, _ inCompletionHandler: (AudioComponentInstance?, OSStatus) -> Void)
@available(OSX 10.6, *)
@discardableResult
func AudioComponentInstanceDispose(_ inInstance: AudioComponentInstance) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func AudioComponentInstanceGetComponent(_ inInstance: AudioComponentInstance) -> AudioComponent
@available(OSX 10.6, *)
@discardableResult
func AudioComponentInstanceCanDo(_ inInstance: AudioComponentInstance, _ inSelectorID: Int16) -> Bool
@available(OSX 10.7, *)
@discardableResult
func AudioComponentRegister(_ inDesc: UnsafePointer<AudioComponentDescription>, _ inName: CFString, _ inVersion: UInt32, _ inFactory: AudioComponentFactoryFunction) -> AudioComponent
@available(OSX 10.7, *)
@discardableResult
func AudioComponentCopyConfigurationInfo(_ inComponent: AudioComponent, _ outConfigurationInfo: UnsafeMutablePointer<Unmanaged<CFDictionary>?>) -> OSStatus
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
@available(OSX 10.7, *)
@discardableResult
func AudioComponentValidate(_ inComponent: AudioComponent, _ inValidationParameters: CFDictionary?, _ outValidationResult: UnsafeMutablePointer<AudioComponentValidationResult>) -> OSStatus
var kAudioComponentValidationParameter_TimeOut: String { get }
var kAudioComponentValidationParameter_ForceValidation: String { get }
