
var kAudioServicesNoError: OSStatus { get }
var kAudioServicesUnsupportedPropertyError: OSStatus { get }
var kAudioServicesBadPropertySizeError: OSStatus { get }
var kAudioServicesBadSpecifierSizeError: OSStatus { get }
var kAudioServicesSystemSoundUnspecifiedError: OSStatus { get }
var kAudioServicesSystemSoundClientTimedOutError: OSStatus { get }
typealias SystemSoundID = UInt32
typealias AudioServicesPropertyID = UInt32
typealias AudioServicesSystemSoundCompletionProc = @convention(c) (SystemSoundID, UnsafeMutablePointer<Void>?) -> Void
var kSystemSoundID_UserPreferredAlert: SystemSoundID { get }
var kSystemSoundID_FlashScreen: SystemSoundID { get }
var kUserPreferredAlert: SystemSoundID { get }
var kAudioServicesPropertyIsUISound: AudioServicesPropertyID { get }
var kAudioServicesPropertyCompletePlaybackIfAppDies: AudioServicesPropertyID { get }
@available(OSX 10.5, *)
@discardableResult
func AudioServicesCreateSystemSoundID(_ inFileURL: CFURL, _ outSystemSoundID: UnsafeMutablePointer<SystemSoundID>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioServicesDisposeSystemSoundID(_ inSystemSoundID: SystemSoundID) -> OSStatus
@available(OSX 10.11, *)
func AudioServicesPlayAlertSoundWithCompletion(_ inSystemSoundID: SystemSoundID, _ inCompletionBlock: (() -> Void)?)
@available(OSX 10.11, *)
func AudioServicesPlaySystemSoundWithCompletion(_ inSystemSoundID: SystemSoundID, _ inCompletionBlock: (() -> Void)?)
@available(OSX 10.5, *)
@discardableResult
func AudioServicesGetPropertyInfo(_ inPropertyID: AudioServicesPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ outPropertyDataSize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioServicesGetProperty(_ inPropertyID: AudioServicesPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioServicesSetProperty(_ inPropertyID: AudioServicesPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ inPropertyDataSize: UInt32, _ inPropertyData: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.5, *)
func AudioServicesPlayAlertSound(_ inSystemSoundID: SystemSoundID)
@available(OSX 10.5, *)
func AudioServicesPlaySystemSound(_ inSystemSoundID: SystemSoundID)
@available(OSX 10.5, *)
@discardableResult
func AudioServicesAddSystemSoundCompletion(_ inSystemSoundID: SystemSoundID, _ inRunLoop: CFRunLoop?, _ inRunLoopMode: CFString?, _ inCompletionRoutine: AudioServicesSystemSoundCompletionProc, _ inClientData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.5, *)
func AudioServicesRemoveSystemSoundCompletion(_ inSystemSoundID: SystemSoundID)
var kAudioHardwareServiceProperty_ServiceRestarted: AudioObjectPropertySelector { get }
var kAudioHardwareServiceDeviceProperty_VirtualMasterVolume: AudioObjectPropertySelector { get }
var kAudioHardwareServiceDeviceProperty_VirtualMasterBalance: AudioObjectPropertySelector { get }
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceHasProperty(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>) -> Bool
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceIsPropertySettable(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ outIsSettable: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceGetPropertyDataSize(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inQualifierDataSize: UInt32, _ inQualifierData: UnsafePointer<Void>, _ outDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceGetPropertyData(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inQualifierDataSize: UInt32, _ inQualifierData: UnsafePointer<Void>, _ ioDataSize: UnsafeMutablePointer<UInt32>, _ outData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceSetPropertyData(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inQualifierDataSize: UInt32, _ inQualifierData: UnsafePointer<Void>, _ inDataSize: UInt32, _ inData: UnsafePointer<Void>) -> OSStatus
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceAddPropertyListener(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inListener: AudioObjectPropertyListenerProc, _ inClientData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX, introduced: 10.5, deprecated: 10.11)
@discardableResult
func AudioHardwareServiceRemovePropertyListener(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inListener: AudioObjectPropertyListenerProc, _ inClientData: UnsafeMutablePointer<Void>) -> OSStatus
