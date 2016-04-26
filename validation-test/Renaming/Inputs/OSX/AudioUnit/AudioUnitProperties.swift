
var kAudioUnitScope_Global: AudioUnitScope { get }
var kAudioUnitScope_Input: AudioUnitScope { get }
var kAudioUnitScope_Output: AudioUnitScope { get }
var kAudioUnitScope_Group: AudioUnitScope { get }
var kAudioUnitScope_Part: AudioUnitScope { get }
var kAudioUnitScope_Note: AudioUnitScope { get }
var kAudioUnitScope_Layer: AudioUnitScope { get }
var kAudioUnitScope_LayerItem: AudioUnitScope { get }
var kAudioUnitProperty_ClassInfo: AudioUnitPropertyID { get }
var kAudioUnitProperty_MakeConnection: AudioUnitPropertyID { get }
var kAudioUnitProperty_SampleRate: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterList: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterInfo: AudioUnitPropertyID { get }
var kAudioUnitProperty_CPULoad: AudioUnitPropertyID { get }
var kAudioUnitProperty_StreamFormat: AudioUnitPropertyID { get }
var kAudioUnitProperty_ElementCount: AudioUnitPropertyID { get }
var kAudioUnitProperty_Latency: AudioUnitPropertyID { get }
var kAudioUnitProperty_SupportedNumChannels: AudioUnitPropertyID { get }
var kAudioUnitProperty_MaximumFramesPerSlice: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterValueStrings: AudioUnitPropertyID { get }
var kAudioUnitProperty_AudioChannelLayout: AudioUnitPropertyID { get }
var kAudioUnitProperty_TailTime: AudioUnitPropertyID { get }
var kAudioUnitProperty_BypassEffect: AudioUnitPropertyID { get }
var kAudioUnitProperty_LastRenderError: AudioUnitPropertyID { get }
var kAudioUnitProperty_SetRenderCallback: AudioUnitPropertyID { get }
var kAudioUnitProperty_FactoryPresets: AudioUnitPropertyID { get }
var kAudioUnitProperty_RenderQuality: AudioUnitPropertyID { get }
var kAudioUnitProperty_HostCallbacks: AudioUnitPropertyID { get }
var kAudioUnitProperty_InPlaceProcessing: AudioUnitPropertyID { get }
var kAudioUnitProperty_ElementName: AudioUnitPropertyID { get }
var kAudioUnitProperty_SupportedChannelLayoutTags: AudioUnitPropertyID { get }
var kAudioUnitProperty_PresentPreset: AudioUnitPropertyID { get }
var kAudioUnitProperty_DependentParameters: AudioUnitPropertyID { get }
var kAudioUnitProperty_InputSamplesInOutput: AudioUnitPropertyID { get }
var kAudioUnitProperty_ShouldAllocateBuffer: AudioUnitPropertyID { get }
var kAudioUnitProperty_FrequencyResponse: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterHistoryInfo: AudioUnitPropertyID { get }
var kAudioUnitProperty_NickName: AudioUnitPropertyID { get }
var kAudioUnitProperty_OfflineRender: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterIDName: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterStringFromValue: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterClumpName: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterValueFromString: AudioUnitPropertyID { get }
var kAudioUnitProperty_ContextName: AudioUnitPropertyID { get }
var kAudioUnitProperty_PresentationLatency: AudioUnitPropertyID { get }
var kAudioUnitProperty_ClassInfoFromDocument: AudioUnitPropertyID { get }
var kAudioUnitProperty_RequestViewController: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParametersForOverview: AudioUnitPropertyID { get }
var kAudioUnitProperty_FastDispatch: AudioUnitPropertyID { get }
var kAudioUnitProperty_SetExternalBuffer: AudioUnitPropertyID { get }
var kAudioUnitProperty_GetUIComponentList: AudioUnitPropertyID { get }
var kAudioUnitProperty_CocoaUI: AudioUnitPropertyID { get }
var kAudioUnitProperty_IconLocation: AudioUnitPropertyID { get }
var kAudioUnitProperty_AUHostIdentifier: AudioUnitPropertyID { get }
var kAudioUnitProperty_MIDIOutputCallbackInfo: AudioUnitPropertyID { get }
var kAudioUnitProperty_MIDIOutputCallback: AudioUnitPropertyID { get }
var kAUPresetVersionKey: String { get }
var kAUPresetTypeKey: String { get }
var kAUPresetSubtypeKey: String { get }
var kAUPresetManufacturerKey: String { get }
var kAUPresetDataKey: String { get }
var kAUPresetNameKey: String { get }
var kAUPresetRenderQualityKey: String { get }
var kAUPresetCPULoadKey: String { get }
var kAUPresetElementNameKey: String { get }
var kAUPresetExternalFileRefs: String { get }
var kAUPresetVSTDataKey: String { get }
var kAUPresetVSTPresetKey: String { get }
var kAUPresetMASDataKey: String { get }
var kAUPresetPartKey: String { get }
struct AudioUnitConnection {
  var sourceAudioUnit: AudioUnit
  var sourceOutputNumber: UInt32
  var destInputNumber: UInt32
}
struct AUChannelInfo {
  var inChannels: Int16
  var outChannels: Int16
  init()
  init(inChannels inChannels: Int16, outChannels outChannels: Int16)
}
struct AudioUnitExternalBuffer {
  var buffer: UnsafeMutablePointer<UInt8>
  var size: UInt32
}
struct AURenderCallbackStruct {
  var inputProc: AURenderCallback
  var inputProcRefCon: UnsafeMutablePointer<Void>?
}
struct AUPreset {
  var presetNumber: Int32
  var presetName: Unmanaged<CFString>
}
var kRenderQuality_Max: Int { get }
var kRenderQuality_High: Int { get }
var kRenderQuality_Medium: Int { get }
var kRenderQuality_Low: Int { get }
var kRenderQuality_Min: Int { get }
var kNumberOfResponseFrequencies: Int { get }
struct AudioUnitFrequencyResponseBin {
  var mFrequency: Float64
  var mMagnitude: Float64
  init()
  init(mFrequency mFrequency: Float64, mMagnitude mMagnitude: Float64)
}
typealias HostCallback_GetBeatAndTempo = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Float64>?, UnsafeMutablePointer<Float64>?) -> OSStatus
typealias HostCallback_GetMusicalTimeLocation = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<UInt32>?, UnsafeMutablePointer<Float32>?, UnsafeMutablePointer<UInt32>?, UnsafeMutablePointer<Float64>?) -> OSStatus
typealias HostCallback_GetTransportState = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<Float64>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<Float64>?, UnsafeMutablePointer<Float64>?) -> OSStatus
typealias HostCallback_GetTransportState2 = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<Float64>?, UnsafeMutablePointer<DarwinBoolean>?, UnsafeMutablePointer<Float64>?, UnsafeMutablePointer<Float64>?) -> OSStatus
struct HostCallbackInfo {
  var hostUserData: UnsafeMutablePointer<Void>?
  var beatAndTempoProc: HostCallback_GetBeatAndTempo?
  var musicalTimeLocationProc: HostCallback_GetMusicalTimeLocation?
  var transportStateProc: HostCallback_GetTransportState?
  var transportStateProc2: HostCallback_GetTransportState2?
  init()
  init(hostUserData hostUserData: UnsafeMutablePointer<Void>?, beatAndTempoProc beatAndTempoProc: HostCallback_GetBeatAndTempo?, musicalTimeLocationProc musicalTimeLocationProc: HostCallback_GetMusicalTimeLocation?, transportStateProc transportStateProc: HostCallback_GetTransportState?, transportStateProc2 transportStateProc2: HostCallback_GetTransportState2?)
}
struct AUDependentParameter {
  var mScope: AudioUnitScope
  var mParameterID: AudioUnitParameterID
  init()
  init(mScope mScope: AudioUnitScope, mParameterID mParameterID: AudioUnitParameterID)
}
struct AudioUnitCocoaViewInfo {
  var mCocoaAUViewBundleLocation: Unmanaged<CFURL>
  var mCocoaAUViewClass: (Unmanaged<CFString>)
}
struct AUHostVersionIdentifier {
  var hostName: Unmanaged<CFString>
  var hostVersion: UInt32
}
typealias AUMIDIOutputCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<AudioTimeStamp>, UInt32, OpaquePointer) -> OSStatus
struct AUMIDIOutputCallbackStruct {
  var midiOutputCallback: AUMIDIOutputCallback
  var userData: UnsafeMutablePointer<Void>?
}
struct AUInputSamplesInOutputCallbackStruct {
  var inputToOutputCallback: AUInputSamplesInOutputCallback
  var userData: UnsafeMutablePointer<Void>?
}
struct AudioUnitParameterHistoryInfo {
  var updatesPerSecond: Float32
  var historyDurationInSeconds: Float32
  init()
  init(updatesPerSecond updatesPerSecond: Float32, historyDurationInSeconds historyDurationInSeconds: Float32)
}
enum AudioUnitParameterUnit : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case generic
  case indexed
  case boolean
  case percent
  case seconds
  case sampleFrames
  case phase
  case rate
  case hertz
  case cents
  case relativeSemiTones
  case midiNoteNumber
  case midiController
  case decibels
  case linearGain
  case degrees
  case equalPowerCrossfade
  case mixerFaderCurve1
  case pan
  case meters
  case absoluteCents
  case octaves
  case BPM
  case beats
  case milliseconds
  case ratio
  case customUnit
}
struct AudioUnitParameterOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var flag_CFNameRelease: AudioUnitParameterOptions { get }
  static var flag_OmitFromPresets: AudioUnitParameterOptions { get }
  static var flag_PlotHistory: AudioUnitParameterOptions { get }
  static var flag_MeterReadOnly: AudioUnitParameterOptions { get }
  static var flag_DisplayMask: AudioUnitParameterOptions { get }
  static var flag_DisplaySquareRoot: AudioUnitParameterOptions { get }
  static var flag_DisplaySquared: AudioUnitParameterOptions { get }
  static var flag_DisplayCubed: AudioUnitParameterOptions { get }
  static var flag_DisplayCubeRoot: AudioUnitParameterOptions { get }
  static var flag_DisplayExponential: AudioUnitParameterOptions { get }
  static var flag_HasClump: AudioUnitParameterOptions { get }
  static var flag_ValuesHaveStrings: AudioUnitParameterOptions { get }
  static var flag_DisplayLogarithmic: AudioUnitParameterOptions { get }
  static var flag_IsHighResolution: AudioUnitParameterOptions { get }
  static var flag_NonRealTime: AudioUnitParameterOptions { get }
  static var flag_CanRamp: AudioUnitParameterOptions { get }
  static var flag_ExpertMode: AudioUnitParameterOptions { get }
  static var flag_HasCFNameString: AudioUnitParameterOptions { get }
  static var flag_IsGlobalMeta: AudioUnitParameterOptions { get }
  static var flag_IsElementMeta: AudioUnitParameterOptions { get }
  static var flag_IsReadable: AudioUnitParameterOptions { get }
  static var flag_IsWritable: AudioUnitParameterOptions { get }
}
struct AudioUnitParameterInfo {
  var name: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  var unitName: Unmanaged<CFString>?
  var clumpID: UInt32
  var cfNameString: Unmanaged<CFString>?
  var unit: AudioUnitParameterUnit
  var minValue: AudioUnitParameterValue
  var maxValue: AudioUnitParameterValue
  var defaultValue: AudioUnitParameterValue
  var flags: AudioUnitParameterOptions
  init()
  init(name name: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8), unitName unitName: Unmanaged<CFString>?, clumpID clumpID: UInt32, cfNameString cfNameString: Unmanaged<CFString>?, unit unit: AudioUnitParameterUnit, minValue minValue: AudioUnitParameterValue, maxValue maxValue: AudioUnitParameterValue, defaultValue defaultValue: AudioUnitParameterValue, flags flags: AudioUnitParameterOptions)
}
var kAudioUnitClumpID_System: Int { get }
@discardableResult
func GetAudioUnitParameterDisplayType(_ flags: AudioUnitParameterOptions) -> AudioUnitParameterOptions
@discardableResult
func SetAudioUnitParameterDisplayType(_ flags: AudioUnitParameterOptions, _ displayType: AudioUnitParameterOptions) -> AudioUnitParameterOptions
var kAudioUnitParameterName_Full: Int { get }
struct AudioUnitParameterNameInfo {
  var inID: AudioUnitParameterID
  var inDesiredLength: Int32
  var outName: Unmanaged<CFString>?
  init()
  init(inID inID: AudioUnitParameterID, inDesiredLength inDesiredLength: Int32, outName outName: Unmanaged<CFString>?)
}
typealias AudioUnitParameterIDName = AudioUnitParameterNameInfo
struct AudioUnitParameterStringFromValue {
  var inParamID: AudioUnitParameterID
  var inValue: UnsafePointer<AudioUnitParameterValue>
  var outString: Unmanaged<CFString>?
}
struct AudioUnitParameterValueFromString {
  var inParamID: AudioUnitParameterID
  var inString: Unmanaged<CFString>
  var outValue: AudioUnitParameterValue
}
var kAudioUnitConfigurationInfo_HasCustomView: String { get }
var kAudioUnitConfigurationInfo_ChannelConfigurations: String { get }
var kAudioUnitConfigurationInfo_InitialInputs: String { get }
var kAudioUnitConfigurationInfo_InitialOutputs: String { get }
var kAudioUnitConfigurationInfo_IconURL: String { get }
var kAudioUnitConfigurationInfo_BusCountWritable: String { get }
var kAudioUnitConfigurationInfo_SupportedChannelLayoutTags: String { get }
var kAudioOutputUnitProperty_IsRunning: AudioUnitPropertyID { get }
var kAudioUnitProperty_AllParameterMIDIMappings: AudioUnitPropertyID { get }
var kAudioUnitProperty_AddParameterMIDIMapping: AudioUnitPropertyID { get }
var kAudioUnitProperty_RemoveParameterMIDIMapping: AudioUnitPropertyID { get }
var kAudioUnitProperty_HotMapParameterMIDIMapping: AudioUnitPropertyID { get }
struct AUParameterMIDIMappingFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var anyChannelFlag: AUParameterMIDIMappingFlags { get }
  static var anyNoteFlag: AUParameterMIDIMappingFlags { get }
  static var subRange: AUParameterMIDIMappingFlags { get }
  static var toggle: AUParameterMIDIMappingFlags { get }
  static var bipolar: AUParameterMIDIMappingFlags { get }
  static var bipolar_On: AUParameterMIDIMappingFlags { get }
}
struct AUParameterMIDIMapping {
  var mScope: AudioUnitScope
  var mElement: AudioUnitElement
  var mParameterID: AudioUnitParameterID
  var mFlags: AUParameterMIDIMappingFlags
  var mSubRangeMin: AudioUnitParameterValue
  var mSubRangeMax: AudioUnitParameterValue
  var mStatus: UInt8
  var mData1: UInt8
  var reserved1: UInt8
  var reserved2: UInt8
  var reserved3: UInt32
  init()
  init(mScope mScope: AudioUnitScope, mElement mElement: AudioUnitElement, mParameterID mParameterID: AudioUnitParameterID, mFlags mFlags: AUParameterMIDIMappingFlags, mSubRangeMin mSubRangeMin: AudioUnitParameterValue, mSubRangeMax mSubRangeMax: AudioUnitParameterValue, mStatus mStatus: UInt8, mData1 mData1: UInt8, reserved1 reserved1: UInt8, reserved2 reserved2: UInt8, reserved3 reserved3: UInt32)
}
var kMusicDeviceProperty_MIDIXMLNames: AudioUnitPropertyID { get }
var kMusicDeviceProperty_PartGroup: AudioUnitPropertyID { get }
var kMusicDeviceProperty_DualSchedulingMode: AudioUnitPropertyID { get }
var kMusicDeviceProperty_SupportsStartStopNote: AudioUnitPropertyID { get }
var kMusicDeviceSampleFrameMask_SampleOffset: Int { get }
var kMusicDeviceSampleFrameMask_IsScheduled: Int { get }
var kAudioUnitOfflineProperty_InputSize: AudioUnitPropertyID { get }
var kAudioUnitOfflineProperty_OutputSize: AudioUnitPropertyID { get }
var kAudioUnitOfflineProperty_StartOffset: AudioUnitPropertyID { get }
var kAudioUnitOfflineProperty_PreflightRequirements: AudioUnitPropertyID { get }
var kAudioUnitOfflineProperty_PreflightName: AudioUnitPropertyID { get }
var kOfflinePreflight_NotRequired: Int { get }
var kOfflinePreflight_Optional: Int { get }
var kOfflinePreflight_Required: Int { get }
var kAudioUnitProperty_DistanceAttenuationData: AudioUnitPropertyID { get }
@available(OSX, introduced: 10.5, deprecated: 10.11)
struct AUDistanceAttenuationData {
  var inNumberOfPairs: UInt32
  init()
}
var kAudioUnitMigrateProperty_FromPlugin: AudioUnitPropertyID { get }
var kAudioUnitMigrateProperty_OldAutomation: AudioUnitPropertyID { get }
var kOtherPluginFormat_Undefined: UInt32 { get }
var kOtherPluginFormat_kMAS: UInt32 { get }
var kOtherPluginFormat_kVST: UInt32 { get }
var kOtherPluginFormat_AU: UInt32 { get }
struct AudioUnitOtherPluginDesc {
  var format: UInt32
  var plugin: AudioClassDescription
  init()
  init(format format: UInt32, plugin plugin: AudioClassDescription)
}
struct AudioUnitParameterValueTranslation {
  var otherDesc: AudioUnitOtherPluginDesc
  var otherParamID: UInt32
  var otherValue: Float32
  var auParamID: AudioUnitParameterID
  var auValue: AudioUnitParameterValue
  init()
  init(otherDesc otherDesc: AudioUnitOtherPluginDesc, otherParamID otherParamID: UInt32, otherValue otherValue: Float32, auParamID auParamID: AudioUnitParameterID, auValue auValue: AudioUnitParameterValue)
}
struct AudioUnitPresetMAS_SettingData {
  var isStockSetting: UInt32
  var settingID: UInt32
  var dataLen: UInt32
  var data: (UInt8)
  init()
  init(isStockSetting isStockSetting: UInt32, settingID settingID: UInt32, dataLen dataLen: UInt32, data data: (UInt8))
}
struct AudioUnitPresetMAS_Settings {
  var manufacturerID: UInt32
  var effectID: UInt32
  var variantID: UInt32
  var settingsVersion: UInt32
  var numberOfSettings: UInt32
  var settings: (AudioUnitPresetMAS_SettingData)
  init()
  init(manufacturerID manufacturerID: UInt32, effectID effectID: UInt32, variantID variantID: UInt32, settingsVersion settingsVersion: UInt32, numberOfSettings numberOfSettings: UInt32, settings settings: (AudioUnitPresetMAS_SettingData))
}
var kAudioUnitProperty_SampleRateConverterComplexity: AudioUnitPropertyID { get }
var kAudioUnitSampleRateConverterComplexity_Linear: UInt32 { get }
var kAudioUnitSampleRateConverterComplexity_Normal: UInt32 { get }
var kAudioUnitSampleRateConverterComplexity_Mastering: UInt32 { get }
var kAudioOutputUnitProperty_CurrentDevice: AudioUnitPropertyID { get }
var kAudioOutputUnitProperty_ChannelMap: AudioUnitPropertyID { get }
var kAudioOutputUnitProperty_EnableIO: AudioUnitPropertyID { get }
var kAudioOutputUnitProperty_StartTime: AudioUnitPropertyID { get }
var kAudioOutputUnitProperty_SetInputCallback: AudioUnitPropertyID { get }
var kAudioOutputUnitProperty_HasIO: AudioUnitPropertyID { get }
var kAudioOutputUnitProperty_StartTimestampsAtZero: AudioUnitPropertyID { get }
struct AudioOutputUnitStartAtTimeParams {
  var mTimestamp: AudioTimeStamp
  var mFlags: UInt32
  init()
  init(mTimestamp mTimestamp: AudioTimeStamp, mFlags mFlags: UInt32)
}
var kAUVoiceIOProperty_BypassVoiceProcessing: AudioUnitPropertyID { get }
var kAUVoiceIOProperty_VoiceProcessingEnableAGC: AudioUnitPropertyID { get }
var kAUVoiceIOProperty_MuteOutput: AudioUnitPropertyID { get }
var kAUVoiceIOProperty_VoiceProcessingQuality: AudioUnitPropertyID { get }
var kAUNBandEQProperty_NumberOfBands: AudioUnitPropertyID { get }
var kAUNBandEQProperty_MaxNumberOfBands: AudioUnitPropertyID { get }
var kAUNBandEQProperty_BiquadCoefficients: AudioUnitPropertyID { get }
var kAUVoiceIOErr_UnexpectedNumberOfInputChannels: OSStatus { get }
var kAudioUnitProperty_MeteringMode: AudioUnitPropertyID { get }
var kAudioUnitProperty_MatrixLevels: AudioUnitPropertyID { get }
var kAudioUnitProperty_MatrixDimensions: AudioUnitPropertyID { get }
var kAudioUnitProperty_MeterClipping: AudioUnitPropertyID { get }
struct AudioUnitMeterClipping {
  var peakValueSinceLastCall: Float32
  var sawInfinity: DarwinBoolean
  var sawNotANumber: DarwinBoolean
  init()
  init(peakValueSinceLastCall peakValueSinceLastCall: Float32, sawInfinity sawInfinity: DarwinBoolean, sawNotANumber sawNotANumber: DarwinBoolean)
}
var kAudioUnitProperty_ReverbRoomType: AudioUnitPropertyID { get }
var kAudioUnitProperty_UsesInternalReverb: AudioUnitPropertyID { get }
var kAudioUnitProperty_SpatializationAlgorithm: AudioUnitPropertyID { get }
var kAudioUnitProperty_SpatialMixerDistanceParams: AudioUnitPropertyID { get }
var kAudioUnitProperty_SpatialMixerAttenuationCurve: AudioUnitPropertyID { get }
var kAudioUnitProperty_SpatialMixerRenderingFlags: AudioUnitPropertyID { get }
enum AUSpatializationAlgorithm : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case spatializationAlgorithm_EqualPowerPanning
  case spatializationAlgorithm_SphericalHead
  case spatializationAlgorithm_HRTF
  case spatializationAlgorithm_SoundField
  case spatializationAlgorithm_VectorBasedPanning
  case spatializationAlgorithm_StereoPassThrough
}
enum AUReverbRoomType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case reverbRoomType_SmallRoom
  case reverbRoomType_MediumRoom
  case reverbRoomType_LargeRoom
  case reverbRoomType_MediumHall
  case reverbRoomType_LargeHall
  case reverbRoomType_Plate
  case reverbRoomType_MediumChamber
  case reverbRoomType_LargeChamber
  case reverbRoomType_Cathedral
  case reverbRoomType_LargeRoom2
  case reverbRoomType_MediumHall2
  case reverbRoomType_MediumHall3
  case reverbRoomType_LargeHall2
}
enum AUSpatialMixerAttenuationCurve : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case spatialMixerAttenuationCurve_Power
  case spatialMixerAttenuationCurve_Exponential
  case spatialMixerAttenuationCurve_Inverse
  case spatialMixerAttenuationCurve_Linear
}
struct MixerDistanceParams {
  var mReferenceDistance: Float32
  var mMaxDistance: Float32
  var mMaxAttenuation: Float32
  init()
  init(mReferenceDistance mReferenceDistance: Float32, mMaxDistance mMaxDistance: Float32, mMaxAttenuation mMaxAttenuation: Float32)
}
struct AUSpatialMixerRenderingFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var spatialMixerRenderingFlags_InterAuralDelay: AUSpatialMixerRenderingFlags { get }
  static var spatialMixerRenderingFlags_DistanceAttenuation: AUSpatialMixerRenderingFlags { get }
}
var kAudioUnitProperty_3DMixerDistanceParams: AudioUnitPropertyID { get }
var kAudioUnitProperty_3DMixerAttenuationCurve: AudioUnitPropertyID { get }
var kAudioUnitProperty_DopplerShift: AudioUnitPropertyID { get }
var kAudioUnitProperty_3DMixerRenderingFlags: AudioUnitPropertyID { get }
var kAudioUnitProperty_3DMixerDistanceAtten: AudioUnitPropertyID { get }
var kAudioUnitProperty_ReverbPreset: AudioUnitPropertyID { get }
struct AU3DMixerRenderingFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var k3DMixerRenderingFlags_InterAuralDelay: AU3DMixerRenderingFlags { get }
  static var k3DMixerRenderingFlags_DopplerShift: AU3DMixerRenderingFlags { get }
  static var k3DMixerRenderingFlags_DistanceAttenuation: AU3DMixerRenderingFlags { get }
  static var k3DMixerRenderingFlags_DistanceFilter: AU3DMixerRenderingFlags { get }
  static var k3DMixerRenderingFlags_DistanceDiffusion: AU3DMixerRenderingFlags { get }
  static var k3DMixerRenderingFlags_LinearDistanceAttenuation: AU3DMixerRenderingFlags { get }
  static var k3DMixerRenderingFlags_ConstantReverbBlend: AU3DMixerRenderingFlags { get }
}
enum AU3DMixerAttenuationCurve : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case k3DMixerAttenuationCurve_Power
  case k3DMixerAttenuationCurve_Exponential
  case k3DMixerAttenuationCurve_Inverse
  case k3DMixerAttenuationCurve_Linear
}
var kAudioUnitProperty_ScheduleAudioSlice: AudioUnitPropertyID { get }
var kAudioUnitProperty_ScheduleStartTimeStamp: AudioUnitPropertyID { get }
var kAudioUnitProperty_CurrentPlayTime: AudioUnitPropertyID { get }
struct AUScheduledAudioSliceFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var scheduledAudioSliceFlag_Complete: AUScheduledAudioSliceFlags { get }
  static var scheduledAudioSliceFlag_BeganToRender: AUScheduledAudioSliceFlags { get }
  static var scheduledAudioSliceFlag_BeganToRenderLate: AUScheduledAudioSliceFlags { get }
  static var scheduledAudioSliceFlag_Loop: AUScheduledAudioSliceFlags { get }
  static var scheduledAudioSliceFlag_Interrupt: AUScheduledAudioSliceFlags { get }
  static var scheduledAudioSliceFlag_InterruptAtLoop: AUScheduledAudioSliceFlags { get }
}
typealias ScheduledAudioSliceCompletionProc = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<ScheduledAudioSlice>) -> Void
struct ScheduledAudioSlice {
  var mTimeStamp: AudioTimeStamp
  var mCompletionProc: ScheduledAudioSliceCompletionProc?
  var mCompletionProcUserData: UnsafeMutablePointer<Void>
  var mFlags: AUScheduledAudioSliceFlags
  var mReserved: UInt32
  var mReserved2: UnsafeMutablePointer<Void>?
  var mNumberFrames: UInt32
  var mBufferList: UnsafeMutablePointer<AudioBufferList>
}
var kAudioUnitProperty_ScheduledFileIDs: AudioUnitPropertyID { get }
var kAudioUnitProperty_ScheduledFileRegion: AudioUnitPropertyID { get }
var kAudioUnitProperty_ScheduledFilePrime: AudioUnitPropertyID { get }
var kAudioUnitProperty_ScheduledFileBufferSizeFrames: AudioUnitPropertyID { get }
var kAudioUnitProperty_ScheduledFileNumberBuffers: AudioUnitPropertyID { get }
typealias ScheduledAudioFileRegionCompletionProc = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<ScheduledAudioFileRegion>, OSStatus) -> Void
struct ScheduledAudioFileRegion {
  var mTimeStamp: AudioTimeStamp
  var mCompletionProc: ScheduledAudioFileRegionCompletionProc?
  var mCompletionProcUserData: UnsafeMutablePointer<Void>?
  var mAudioFile: OpaquePointer
  var mLoopCount: UInt32
  var mStartFrame: Int64
  var mFramesToPlay: UInt32
}
var kMusicDeviceProperty_UsesInternalReverb: AudioUnitPropertyID { get }
var kMusicDeviceProperty_SoundBankData: AudioUnitPropertyID { get }
var kMusicDeviceProperty_StreamFromDisk: AudioUnitPropertyID { get }
var kMusicDeviceProperty_SoundBankFSRef: AudioUnitPropertyID { get }
var kMusicDeviceProperty_InstrumentName: AudioUnitPropertyID { get }
var kMusicDeviceProperty_InstrumentNumber: AudioUnitPropertyID { get }
var kMusicDeviceProperty_InstrumentCount: AudioUnitPropertyID { get }
var kMusicDeviceProperty_BankName: AudioUnitPropertyID { get }
var kMusicDeviceProperty_SoundBankURL: AudioUnitPropertyID { get }
var kAUMIDISynthProperty_EnablePreload: AudioUnitPropertyID { get }
var kAUSamplerProperty_LoadInstrument: AudioUnitPropertyID { get }
var kAUSamplerProperty_LoadAudioFiles: AudioUnitPropertyID { get }
struct AUSamplerInstrumentData {
  var fileURL: Unmanaged<CFURL>
  var instrumentType: UInt8
  var bankMSB: UInt8
  var bankLSB: UInt8
  var presetID: UInt8
}
var kInstrumentType_DLSPreset: Int { get }
var kInstrumentType_SF2Preset: Int { get }
var kInstrumentType_AUPreset: Int { get }
var kInstrumentType_Audiofile: Int { get }
var kInstrumentType_EXS24: Int { get }
var kAUSampler_DefaultPercussionBankMSB: Int { get }
var kAUSampler_DefaultMelodicBankMSB: Int { get }
var kAUSampler_DefaultBankLSB: Int { get }
var kAudioUnitProperty_DeferredRendererPullSize: AudioUnitPropertyID { get }
var kAudioUnitProperty_DeferredRendererExtraLatency: AudioUnitPropertyID { get }
var kAudioUnitProperty_DeferredRendererWaitFrames: AudioUnitPropertyID { get }
var kAUNetReceiveProperty_Hostname: AudioUnitPropertyID { get }
var kAUNetReceiveProperty_Password: AudioUnitPropertyID { get }
var kAUNetSendProperty_PortNum: AudioUnitPropertyID { get }
var kAUNetSendProperty_TransmissionFormat: AudioUnitPropertyID { get }
var kAUNetSendProperty_TransmissionFormatIndex: AudioUnitPropertyID { get }
var kAUNetSendProperty_ServiceName: AudioUnitPropertyID { get }
var kAUNetSendProperty_Disconnect: AudioUnitPropertyID { get }
var kAUNetSendProperty_Password: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_PCMFloat32: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_PCMInt24: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_PCMInt16: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_Lossless24: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_Lossless16: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_ULaw: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_IMA4: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_128kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_96kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_80kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_64kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_48kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_40kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_32kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_LD_64kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_LD_48kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_LD_40kbpspc: AudioUnitPropertyID { get }
var kAUNetSendPresetFormat_AAC_LD_32kbpspc: AudioUnitPropertyID { get }
var kAUNetSendNumPresetFormats: AudioUnitPropertyID { get }
struct AUNumVersion {
  var nonRelRev: UInt8
  var stage: UInt8
  var minorAndBugRev: UInt8
  var majorRev: UInt8
  init()
  init(nonRelRev nonRelRev: UInt8, stage stage: UInt8, minorAndBugRev minorAndBugRev: UInt8, majorRev majorRev: UInt8)
}
struct AUHostIdentifier {
  var hostName: Unmanaged<CFString>
  var hostVersion: AUNumVersion
}
var kAudioUnitParameterFlag_Global: Int { get }
var kAudioUnitParameterFlag_Input: Int { get }
var kAudioUnitParameterFlag_Output: Int { get }
var kAudioUnitParameterFlag_Group: Int { get }
var kAudioUnitParameterFlag_HasName: Int { get }
var kAudioUnitProperty_SRCAlgorithm: AudioUnitPropertyID { get }
var kAudioUnitProperty_MIDIControlMapping: AudioUnitPropertyID { get }
var kAudioUnitProperty_CurrentPreset: AudioUnitPropertyID { get }
var kAudioUnitProperty_ParameterValueName: AudioUnitPropertyID { get }
var kAudioUnitProperty_BusCount: AudioUnitPropertyID { get }
var kAudioOfflineUnitProperty_InputSize: AudioUnitPropertyID { get }
var kAudioOfflineUnitProperty_OutputSize: AudioUnitPropertyID { get }
var kAudioUnitSRCAlgorithm_Polyphase: UInt32 { get }
var kAudioUnitSRCAlgorithm_MediumQuality: UInt32 { get }
struct AudioUnitMIDIControlMapping {
  var midiNRPN: UInt16
  var midiControl: UInt8
  var scope: UInt8
  var element: AudioUnitElement
  var parameter: AudioUnitParameterID
  init()
  init(midiNRPN midiNRPN: UInt16, midiControl midiControl: UInt8, scope scope: UInt8, element element: AudioUnitElement, parameter parameter: AudioUnitParameterID)
}
struct AudioUnitParameterValueName {
  var inParamID: AudioUnitParameterID
  var inValue: UnsafePointer<Float32>
  var outName: Unmanaged<CFString>
}
var kMusicDeviceProperty_GroupOutputBus: AudioUnitPropertyID { get }
var kMusicDeviceProperty_SoundBankFSSpec: AudioUnitPropertyID { get }
var kAudioUnitProperty_PannerMode: AudioUnitPropertyID { get }
var kAudioUnitProperty_SpeakerConfiguration: AudioUnitPropertyID { get }
var kSpeakerConfiguration_HeadPhones: Int { get }
var kSpeakerConfiguration_Stereo: Int { get }
var kSpeakerConfiguration_Quad: Int { get }
var kSpeakerConfiguration_5_0: Int { get }
var kSpeakerConfiguration_5_1: Int { get }
struct AUSamplerBankPresetData {
  var bankURL: Unmanaged<CFURL>
  var bankMSB: UInt8
  var bankLSB: UInt8
  var presetID: UInt8
  var reserved: UInt8
}
var kAUSamplerProperty_LoadPresetFromBank: AudioUnitPropertyID { get }
var kAUSamplerProperty_BankAndPreset: AudioUnitPropertyID { get }
