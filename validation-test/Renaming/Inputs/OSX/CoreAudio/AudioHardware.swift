
var kAudioObjectSystemObject: Int32 { get }
typealias AudioObjectPropertyListenerProc = @convention(c) (AudioObjectID, UInt32, UnsafePointer<AudioObjectPropertyAddress>, UnsafeMutablePointer<Void>?) -> OSStatus
typealias AudioObjectPropertyListenerBlock = (UInt32, UnsafePointer<AudioObjectPropertyAddress>) -> Void
var kAudioObjectPropertyCreator: AudioObjectPropertySelector { get }
var kAudioObjectPropertyListenerAdded: AudioObjectPropertySelector { get }
var kAudioObjectPropertyListenerRemoved: AudioObjectPropertySelector { get }
@available(OSX 10.4, *)
func AudioObjectShow(_ inObjectID: AudioObjectID)
@available(OSX 10.4, *)
@discardableResult
func AudioObjectHasProperty(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>) -> Bool
@available(OSX 10.4, *)
@discardableResult
func AudioObjectIsPropertySettable(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ outIsSettable: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioObjectGetPropertyDataSize(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inQualifierDataSize: UInt32, _ inQualifierData: UnsafePointer<Void>?, _ outDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioObjectGetPropertyData(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inQualifierDataSize: UInt32, _ inQualifierData: UnsafePointer<Void>?, _ ioDataSize: UnsafeMutablePointer<UInt32>, _ outData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioObjectSetPropertyData(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inQualifierDataSize: UInt32, _ inQualifierData: UnsafePointer<Void>?, _ inDataSize: UInt32, _ inData: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioObjectAddPropertyListener(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inListener: AudioObjectPropertyListenerProc, _ inClientData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioObjectRemovePropertyListener(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inListener: AudioObjectPropertyListenerProc, _ inClientData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func AudioObjectAddPropertyListenerBlock(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inDispatchQueue: dispatch_queue_t?, _ inListener: AudioObjectPropertyListenerBlock) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func AudioObjectRemovePropertyListenerBlock(_ inObjectID: AudioObjectID, _ inAddress: UnsafePointer<AudioObjectPropertyAddress>, _ inDispatchQueue: dispatch_queue_t?, _ inListener: AudioObjectPropertyListenerBlock) -> OSStatus
var kAudioSystemObjectClassID: AudioClassID { get }
enum AudioHardwarePowerHint : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case none
  case favorSavingPower
}
var kAudioHardwarePropertyDevices: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyDefaultInputDevice: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyDefaultOutputDevice: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyDefaultSystemOutputDevice: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyTranslateUIDToDevice: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyMixStereoToMono: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyPlugInList: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyTranslateBundleIDToPlugIn: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyTransportManagerList: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyTranslateBundleIDToTransportManager: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyBoxList: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyTranslateUIDToBox: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyProcessIsMaster: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyIsInitingOrExiting: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyUserIDChanged: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyProcessIsAudible: AudioObjectPropertySelector { get }
var kAudioHardwarePropertySleepingIsAllowed: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyUnloadingIsAllowed: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyHogModeIsAllowed: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyUserSessionIsActiveOrHeadless: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyServiceRestarted: AudioObjectPropertySelector { get }
var kAudioHardwarePropertyPowerHint: AudioObjectPropertySelector { get }
@available(OSX 10.1, *)
@discardableResult
func AudioHardwareUnload() -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func AudioHardwareCreateAggregateDevice(_ inDescription: CFDictionary, _ outDeviceID: UnsafeMutablePointer<AudioObjectID>) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func AudioHardwareDestroyAggregateDevice(_ inDeviceID: AudioObjectID) -> OSStatus
var kAudioPlugInCreateAggregateDevice: AudioObjectPropertySelector { get }
var kAudioPlugInDestroyAggregateDevice: AudioObjectPropertySelector { get }
var kAudioTransportManagerCreateEndPointDevice: AudioObjectPropertySelector { get }
var kAudioTransportManagerDestroyEndPointDevice: AudioObjectPropertySelector { get }
typealias AudioDeviceIOProc = @convention(c) (AudioObjectID, UnsafePointer<AudioTimeStamp>, UnsafePointer<AudioBufferList>, UnsafePointer<AudioTimeStamp>, UnsafeMutablePointer<AudioBufferList>, UnsafePointer<AudioTimeStamp>, UnsafeMutablePointer<Void>?) -> OSStatus
typealias AudioDeviceIOBlock = (UnsafePointer<AudioTimeStamp>, UnsafePointer<AudioBufferList>, UnsafePointer<AudioTimeStamp>, UnsafeMutablePointer<AudioBufferList>, UnsafePointer<AudioTimeStamp>) -> Void
typealias AudioDeviceIOProcID = AudioDeviceIOProc
struct AudioHardwareIOProcStreamUsage {
  var mIOProc: UnsafeMutablePointer<Void>
  var mNumberStreams: UInt32
  var mStreamIsOn: (UInt32)
}
var kAudioDeviceStartTimeIsInputFlag: UInt32 { get }
var kAudioDeviceStartTimeDontConsultDeviceFlag: UInt32 { get }
var kAudioDeviceStartTimeDontConsultHALFlag: UInt32 { get }
var kAudioDevicePropertyPlugIn: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceHasChanged: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDeviceIsRunningSomewhere: AudioObjectPropertySelector { get }
var kAudioDeviceProcessorOverload: AudioObjectPropertySelector { get }
var kAudioDevicePropertyIOStoppedAbnormally: AudioObjectPropertySelector { get }
var kAudioDevicePropertyHogMode: AudioObjectPropertySelector { get }
var kAudioDevicePropertyBufferFrameSize: AudioObjectPropertySelector { get }
var kAudioDevicePropertyBufferFrameSizeRange: AudioObjectPropertySelector { get }
var kAudioDevicePropertyUsesVariableBufferFrameSizes: AudioObjectPropertySelector { get }
var kAudioDevicePropertyIOCycleUsage: AudioObjectPropertySelector { get }
var kAudioDevicePropertyStreamConfiguration: AudioObjectPropertySelector { get }
var kAudioDevicePropertyIOProcStreamUsage: AudioObjectPropertySelector { get }
var kAudioDevicePropertyActualSampleRate: AudioObjectPropertySelector { get }
var kAudioDevicePropertyJackIsConnected: AudioObjectPropertySelector { get }
var kAudioDevicePropertyVolumeScalar: AudioObjectPropertySelector { get }
var kAudioDevicePropertyVolumeDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyVolumeRangeDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyVolumeScalarToDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyVolumeDecibelsToScalar: AudioObjectPropertySelector { get }
var kAudioDevicePropertyStereoPan: AudioObjectPropertySelector { get }
var kAudioDevicePropertyStereoPanChannels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyMute: AudioObjectPropertySelector { get }
var kAudioDevicePropertySolo: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPhantomPower: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPhaseInvert: AudioObjectPropertySelector { get }
var kAudioDevicePropertyClipLight: AudioObjectPropertySelector { get }
var kAudioDevicePropertyTalkback: AudioObjectPropertySelector { get }
var kAudioDevicePropertyListenback: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDataSource: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDataSources: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDataSourceNameForIDCFString: AudioObjectPropertySelector { get }
var kAudioDevicePropertyDataSourceKindForID: AudioObjectPropertySelector { get }
var kAudioDevicePropertyClockSource: AudioObjectPropertySelector { get }
var kAudioDevicePropertyClockSources: AudioObjectPropertySelector { get }
var kAudioDevicePropertyClockSourceNameForIDCFString: AudioObjectPropertySelector { get }
var kAudioDevicePropertyClockSourceKindForID: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThru: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruSolo: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruVolumeScalar: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruVolumeDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruVolumeRangeDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruVolumeScalarToDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruVolumeDecibelsToScalar: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruStereoPan: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruStereoPanChannels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruDestination: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruDestinations: AudioObjectPropertySelector { get }
var kAudioDevicePropertyPlayThruDestinationNameForIDCFString: AudioObjectPropertySelector { get }
var kAudioDevicePropertyChannelNominalLineLevel: AudioObjectPropertySelector { get }
var kAudioDevicePropertyChannelNominalLineLevels: AudioObjectPropertySelector { get }
var kAudioDevicePropertyChannelNominalLineLevelNameForIDCFString: AudioObjectPropertySelector { get }
var kAudioDevicePropertyHighPassFilterSetting: AudioObjectPropertySelector { get }
var kAudioDevicePropertyHighPassFilterSettings: AudioObjectPropertySelector { get }
var kAudioDevicePropertyHighPassFilterSettingNameForIDCFString: AudioObjectPropertySelector { get }
var kAudioDevicePropertySubVolumeScalar: AudioObjectPropertySelector { get }
var kAudioDevicePropertySubVolumeDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertySubVolumeRangeDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertySubVolumeScalarToDecibels: AudioObjectPropertySelector { get }
var kAudioDevicePropertySubVolumeDecibelsToScalar: AudioObjectPropertySelector { get }
var kAudioDevicePropertySubMute: AudioObjectPropertySelector { get }
@available(OSX 10.5, *)
@discardableResult
func AudioDeviceCreateIOProcID(_ inDevice: AudioObjectID, _ inProc: AudioDeviceIOProc, _ inClientData: UnsafeMutablePointer<Void>?, _ outIOProcID: UnsafeMutablePointer<AudioDeviceIOProcID?>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func AudioDeviceCreateIOProcIDWithBlock(_ outIOProcID: UnsafeMutablePointer<AudioDeviceIOProcID?>, _ inDevice: AudioObjectID, _ inDispatchQueue: dispatch_queue_t?, _ inIOBlock: AudioDeviceIOBlock) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioDeviceDestroyIOProcID(_ inDevice: AudioObjectID, _ inIOProcID: AudioDeviceIOProcID) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func AudioDeviceStart(_ inDevice: AudioObjectID, _ inProcID: AudioDeviceIOProcID?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func AudioDeviceStartAtTime(_ inDevice: AudioObjectID, _ inProcID: AudioDeviceIOProcID?, _ ioRequestedStartTime: UnsafeMutablePointer<AudioTimeStamp>, _ inFlags: UInt32) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func AudioDeviceStop(_ inDevice: AudioObjectID, _ inProcID: AudioDeviceIOProcID?) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func AudioDeviceGetCurrentTime(_ inDevice: AudioObjectID, _ outTime: UnsafeMutablePointer<AudioTimeStamp>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func AudioDeviceTranslateTime(_ inDevice: AudioObjectID, _ inTime: UnsafePointer<AudioTimeStamp>, _ outTime: UnsafeMutablePointer<AudioTimeStamp>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func AudioDeviceGetNearestStartTime(_ inDevice: AudioObjectID, _ ioRequestedStartTime: UnsafeMutablePointer<AudioTimeStamp>, _ inFlags: UInt32) -> OSStatus
var kAudioAggregateDeviceClassID: AudioClassID { get }
var kAudioAggregateDeviceUIDKey: String { get }
var kAudioAggregateDeviceNameKey: String { get }
var kAudioAggregateDeviceSubDeviceListKey: String { get }
var kAudioAggregateDeviceMasterSubDeviceKey: String { get }
var kAudioAggregateDeviceIsPrivateKey: String { get }
var kAudioAggregateDeviceIsStackedKey: String { get }
var kAudioAggregateDevicePropertyFullSubDeviceList: AudioObjectPropertySelector { get }
var kAudioAggregateDevicePropertyActiveSubDeviceList: AudioObjectPropertySelector { get }
var kAudioAggregateDevicePropertyComposition: AudioObjectPropertySelector { get }
var kAudioAggregateDevicePropertyMasterSubDevice: AudioObjectPropertySelector { get }
var kAudioSubDeviceClassID: AudioClassID { get }
var kAudioSubDeviceDriftCompensationMinQuality: UInt32 { get }
var kAudioSubDeviceDriftCompensationLowQuality: UInt32 { get }
var kAudioSubDeviceDriftCompensationMediumQuality: UInt32 { get }
var kAudioSubDeviceDriftCompensationHighQuality: UInt32 { get }
var kAudioSubDeviceDriftCompensationMaxQuality: UInt32 { get }
var kAudioSubDeviceUIDKey: String { get }
var kAudioSubDeviceNameKey: String { get }
var kAudioSubDeviceInputChannelsKey: String { get }
var kAudioSubDeviceOutputChannelsKey: String { get }
var kAudioSubDeviceExtraInputLatencyKey: String { get }
var kAudioSubDeviceExtraOutputLatencyKey: String { get }
var kAudioSubDeviceDriftCompensationKey: String { get }
var kAudioSubDeviceDriftCompensationQualityKey: String { get }
var kAudioSubDevicePropertyExtraLatency: AudioObjectPropertySelector { get }
var kAudioSubDevicePropertyDriftCompensation: AudioObjectPropertySelector { get }
var kAudioSubDevicePropertyDriftCompensationQuality: AudioObjectPropertySelector { get }
