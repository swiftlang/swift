
typealias CMIODeviceID = CMIOObjectID
typealias CMIODevicePropertyID = CMIOObjectPropertySelector
struct CMIODeviceStreamConfiguration {
  var mNumberStreams: UInt32
  init()
}
struct CMIODeviceAVCCommand {
  var mCommand: UnsafeMutablePointer<UInt8>!
  var mCommandLength: UInt32
  var mResponse: UnsafeMutablePointer<UInt8>!
  var mResponseLength: UInt32
  var mResponseUsed: UInt32
  init()
  init(mCommand mCommand: UnsafeMutablePointer<UInt8>!, mCommandLength mCommandLength: UInt32, mResponse mResponse: UnsafeMutablePointer<UInt8>!, mResponseLength mResponseLength: UInt32, mResponseUsed mResponseUsed: UInt32)
}
struct CMIODeviceRS422Command {
  var mCommand: UnsafeMutablePointer<UInt8>!
  var mCommandLength: UInt32
  var mResponse: UnsafeMutablePointer<UInt8>!
  var mResponseLength: UInt32
  var mResponseUsed: UInt32
  init()
  init(mCommand mCommand: UnsafeMutablePointer<UInt8>!, mCommandLength mCommandLength: UInt32, mResponse mResponse: UnsafeMutablePointer<UInt8>!, mResponseLength mResponseLength: UInt32, mResponseUsed mResponseUsed: UInt32)
}
typealias CMIODeviceGetSMPTETimeProc = @convention(c) (UnsafeMutablePointer<Void>!, UnsafeMutablePointer<UInt64>!, UnsafeMutablePointer<DarwinBoolean>!, UnsafeMutablePointer<UInt32>!) -> OSStatus
struct CMIODeviceSMPTETimeCallback {
  var mGetSMPTETimeProc: CMIODeviceGetSMPTETimeProc!
  var mRefCon: UnsafeMutablePointer<Void>!
  init()
  init(mGetSMPTETimeProc mGetSMPTETimeProc: CMIODeviceGetSMPTETimeProc!, mRefCon mRefCon: UnsafeMutablePointer<Void>!)
}
var kCMIODevicePropertyScopeInput: Int { get }
var kCMIODevicePropertyScopeOutput: Int { get }
var kCMIODevicePropertyScopePlayThrough: Int { get }
var kCMIODeviceClassID: Int { get }
var kCMIODeviceUnknown: Int { get }
var kCMIOAVCDeviceType_Unknown: Int { get }
var kCMIOAVCDeviceType_DV_NTSC: Int { get }
var kCMIOAVCDeviceType_DV_PAL: Int { get }
var kCMIOAVCDeviceType_DVCPro_NTSC: Int { get }
var kCMIOAVCDeviceType_DVCPro_PAL: Int { get }
var kCMIOAVCDeviceType_DVCPro50_NTSC: Int { get }
var kCMIOAVCDeviceType_DVCPro50_PAL: Int { get }
var kCMIOAVCDeviceType_DVCPro100_NTSC: Int { get }
var kCMIOAVCDeviceType_DVCPro100_PAL: Int { get }
var kCMIOAVCDeviceType_DVCPro100_720p: Int { get }
var kCMIOAVCDeviceType_DVCProHD_1080i50: Int { get }
var kCMIOAVCDeviceType_DVCProHD_1080i60: Int { get }
var kCMIOAVCDeviceType_MPEG2: Int { get }
var kCMIODeviceAVCSignalModeSD525_60: Int { get }
var kCMIODeviceAVCSignalModeSDL525_60: Int { get }
var kCMIODeviceAVCSignalModeHD1125_60: Int { get }
var kCMIODeviceAVCSignalModeSD625_50: Int { get }
var kCMIODeviceAVCSignalModeSDL625_50: Int { get }
var kCMIODeviceAVCSignalModeHD1250_50: Int { get }
var kCMIODeviceAVCSignalModeMPEG25Mbps_60: Int { get }
var kCMIODeviceAVCSignalModeHDV1_60: Int { get }
var kCMIODeviceAVCSignalModeMPEG12Mbps_60: Int { get }
var kCMIODeviceAVCSignalModeMPEG6Mbps_60: Int { get }
var kCMIODeviceAVCSignalModeMPEG25Mbps_50: Int { get }
var kCMIODeviceAVCSignalModeHDV1_50: Int { get }
var kCMIODeviceAVCSignalModeMPEG12Mbps_50: Int { get }
var kCMIODeviceAVCSignalModeMPEG6Mbps_50: Int { get }
var kCMIODeviceAVCSignalModeDVHS: Int { get }
var kCMIODeviceAVCSignalModeVHSNTSC: Int { get }
var kCMIODeviceAVCSignalModeVHSMPAL: Int { get }
var kCMIODeviceAVCSignalModeVHSPAL: Int { get }
var kCMIODeviceAVCSignalModeVHSNPAL: Int { get }
var kCMIODeviceAVCSignalModeVHSSECAM: Int { get }
var kCMIODeviceAVCSignalModeVHSMESECAM: Int { get }
var kCMIODeviceAVCSignalModeSVHS525_60: Int { get }
var kCMIODeviceAVCSignalModeSVHS625_50: Int { get }
var kCMIODeviceAVCSignalMode8mmNTSC: Int { get }
var kCMIODeviceAVCSignalMode8mmPAL: Int { get }
var kCMIODeviceAVCSignalModeHi8NTSC: Int { get }
var kCMIODeviceAVCSignalModeHi8PAL: Int { get }
var kCMIODeviceAVCSignalModeMicroMV12Mbps_60: Int { get }
var kCMIODeviceAVCSignalModeMicroMV6Mbps_60: Int { get }
var kCMIODeviceAVCSignalModeMicroMV12Mbps_50: Int { get }
var kCMIODeviceAVCSignalModeMicroMV6Mbps_50: Int { get }
var kCMIODeviceAVCSignalModeAudio: Int { get }
var kCMIODeviceAVCSignalModeHDV2_60: Int { get }
var kCMIODeviceAVCSignalModeHDV2_50: Int { get }
var kCMIODeviceAVCSignalModeDVCPro25_625_50: Int { get }
var kCMIODeviceAVCSignalModeDVCPro25_525_60: Int { get }
var kCMIODeviceAVCSignalModeDVCPro50_625_50: Int { get }
var kCMIODeviceAVCSignalModeDVCPro50_525_60: Int { get }
var kCMIODeviceAVCSignalModeDVCPro100_50: Int { get }
var kCMIODeviceAVCSignalModeDVCPro100_60: Int { get }
var kCMIODevicePropertyPlugIn: Int { get }
var kCMIODevicePropertyDeviceUID: Int { get }
var kCMIODevicePropertyModelUID: Int { get }
var kCMIODevicePropertyTransportType: Int { get }
var kCMIODevicePropertyDeviceIsAlive: Int { get }
var kCMIODevicePropertyDeviceHasChanged: Int { get }
var kCMIODevicePropertyDeviceIsRunning: Int { get }
var kCMIODevicePropertyDeviceIsRunningSomewhere: Int { get }
var kCMIODevicePropertyDeviceCanBeDefaultDevice: Int { get }
var kCMIODevicePropertyHogMode: Int { get }
var kCMIODevicePropertyLatency: Int { get }
var kCMIODevicePropertyStreams: Int { get }
var kCMIODevicePropertyStreamConfiguration: Int { get }
var kCMIODevicePropertyDeviceMaster: Int { get }
var kCMIODevicePropertyExcludeNonDALAccess: Int { get }
var kCMIODevicePropertyClientSyncDiscontinuity: Int { get }
var kCMIODevicePropertySMPTETimeCallback: Int { get }
var kCMIODevicePropertyCanProcessAVCCommand: Int { get }
var kCMIODevicePropertyAVCDeviceType: Int { get }
var kCMIODevicePropertyAVCDeviceSignalMode: Int { get }
var kCMIODevicePropertyCanProcessRS422Command: Int { get }
var kCMIODevicePropertyLinkedCoreAudioDeviceUID: Int { get }
var kCMIODevicePropertyVideoDigitizerComponents: Int { get }
var kCMIODevicePropertySuspendedByUser: Int { get }
var kCMIODevicePropertyLinkedAndSyncedCoreAudioDeviceUID: Int { get }
var kCMIODevicePropertyIIDCInitialUnitSpace: Int { get }
var kCMIODevicePropertyIIDCCSRData: Int { get }
var kCMIODevicePropertyCanSwitchFrameRatesWithoutFrameDrops: Int { get }
@available(OSX 10.7, *)
@discardableResult
func CMIODeviceStartStream(_ deviceID: CMIODeviceID, _ streamID: CMIOStreamID) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIODeviceStopStream(_ deviceID: CMIODeviceID, _ streamID: CMIOStreamID) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIODeviceProcessAVCCommand(_ deviceID: CMIODeviceID, _ ioAVCCommand: UnsafeMutablePointer<CMIODeviceAVCCommand>!) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CMIODeviceProcessRS422Command(_ deviceID: CMIODeviceID, _ ioRS422Command: UnsafeMutablePointer<CMIODeviceRS422Command>!) -> OSStatus
