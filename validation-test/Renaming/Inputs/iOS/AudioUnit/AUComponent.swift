
typealias AudioUnit = AudioComponentInstance
var kAudioUnitType_Output: UInt32 { get }
var kAudioUnitType_MusicDevice: UInt32 { get }
var kAudioUnitType_MusicEffect: UInt32 { get }
var kAudioUnitType_FormatConverter: UInt32 { get }
var kAudioUnitType_Effect: UInt32 { get }
var kAudioUnitType_Mixer: UInt32 { get }
var kAudioUnitType_Panner: UInt32 { get }
var kAudioUnitType_Generator: UInt32 { get }
var kAudioUnitType_OfflineEffect: UInt32 { get }
var kAudioUnitType_MIDIProcessor: UInt32 { get }
var kAudioUnitType_RemoteEffect: UInt32 { get }
var kAudioUnitType_RemoteGenerator: UInt32 { get }
var kAudioUnitType_RemoteInstrument: UInt32 { get }
var kAudioUnitType_RemoteMusicEffect: UInt32 { get }
var kAudioUnitManufacturer_Apple: UInt32 { get }
var kAudioUnitSubType_GenericOutput: UInt32 { get }
var kAudioUnitSubType_VoiceProcessingIO: UInt32 { get }
var kAudioUnitSubType_RemoteIO: UInt32 { get }
var kAudioUnitSubType_Sampler: UInt32 { get }
var kAudioUnitSubType_MIDISynth: UInt32 { get }
var kAudioUnitSubType_AUConverter: UInt32 { get }
var kAudioUnitSubType_Varispeed: UInt32 { get }
var kAudioUnitSubType_DeferredRenderer: UInt32 { get }
var kAudioUnitSubType_Splitter: UInt32 { get }
var kAudioUnitSubType_MultiSplitter: UInt32 { get }
var kAudioUnitSubType_Merger: UInt32 { get }
var kAudioUnitSubType_NewTimePitch: UInt32 { get }
var kAudioUnitSubType_AUiPodTimeOther: UInt32 { get }
var kAudioUnitSubType_RoundTripAAC: UInt32 { get }
var kAudioUnitSubType_AUiPodTime: UInt32 { get }
var kAudioUnitSubType_PeakLimiter: UInt32 { get }
var kAudioUnitSubType_DynamicsProcessor: UInt32 { get }
var kAudioUnitSubType_LowPassFilter: UInt32 { get }
var kAudioUnitSubType_HighPassFilter: UInt32 { get }
var kAudioUnitSubType_BandPassFilter: UInt32 { get }
var kAudioUnitSubType_HighShelfFilter: UInt32 { get }
var kAudioUnitSubType_LowShelfFilter: UInt32 { get }
var kAudioUnitSubType_ParametricEQ: UInt32 { get }
var kAudioUnitSubType_Distortion: UInt32 { get }
var kAudioUnitSubType_Delay: UInt32 { get }
var kAudioUnitSubType_SampleDelay: UInt32 { get }
var kAudioUnitSubType_NBandEQ: UInt32 { get }
var kAudioUnitSubType_Reverb2: UInt32 { get }
var kAudioUnitSubType_AUiPodEQ: UInt32 { get }
var kAudioUnitSubType_MultiChannelMixer: UInt32 { get }
var kAudioUnitSubType_MatrixMixer: UInt32 { get }
var kAudioUnitSubType_SpatialMixer: UInt32 { get }
@available(*, deprecated, message: "renamed to kAudioUnitSubType_SpatialMixer")
var kAudioUnitSubType_AU3DMixerEmbedded: UInt32 { get }
var kAudioUnitSubType_ScheduledSoundPlayer: UInt32 { get }
var kAudioUnitSubType_AudioFilePlayer: UInt32 { get }
struct AudioUnitRenderActionFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var unitRenderAction_PreRender: AudioUnitRenderActionFlags { get }
  static var unitRenderAction_PostRender: AudioUnitRenderActionFlags { get }
  static var unitRenderAction_OutputIsSilence: AudioUnitRenderActionFlags { get }
  static var offlineUnitRenderAction_Preflight: AudioUnitRenderActionFlags { get }
  static var offlineUnitRenderAction_Render: AudioUnitRenderActionFlags { get }
  static var offlineUnitRenderAction_Complete: AudioUnitRenderActionFlags { get }
  static var unitRenderAction_PostRenderError: AudioUnitRenderActionFlags { get }
  static var unitRenderAction_DoNotCheckRenderArgs: AudioUnitRenderActionFlags { get }
}
var kAudioUnitErr_InvalidProperty: OSStatus { get }
var kAudioUnitErr_InvalidParameter: OSStatus { get }
var kAudioUnitErr_InvalidElement: OSStatus { get }
var kAudioUnitErr_NoConnection: OSStatus { get }
var kAudioUnitErr_FailedInitialization: OSStatus { get }
var kAudioUnitErr_TooManyFramesToProcess: OSStatus { get }
var kAudioUnitErr_InvalidFile: OSStatus { get }
var kAudioUnitErr_UnknownFileType: OSStatus { get }
var kAudioUnitErr_FileNotSpecified: OSStatus { get }
var kAudioUnitErr_FormatNotSupported: OSStatus { get }
var kAudioUnitErr_Uninitialized: OSStatus { get }
var kAudioUnitErr_InvalidScope: OSStatus { get }
var kAudioUnitErr_PropertyNotWritable: OSStatus { get }
var kAudioUnitErr_CannotDoInCurrentContext: OSStatus { get }
var kAudioUnitErr_InvalidPropertyValue: OSStatus { get }
var kAudioUnitErr_PropertyNotInUse: OSStatus { get }
var kAudioUnitErr_Initialized: OSStatus { get }
var kAudioUnitErr_InvalidOfflineRender: OSStatus { get }
var kAudioUnitErr_Unauthorized: OSStatus { get }
var kAudioComponentErr_InstanceInvalidated: OSStatus { get }
var kAudioComponentErr_DuplicateDescription: OSStatus { get }
var kAudioComponentErr_UnsupportedType: OSStatus { get }
var kAudioComponentErr_TooManyInstances: OSStatus { get }
var kAudioComponentErr_NotPermitted: OSStatus { get }
var kAudioComponentErr_InitializationTimedOut: OSStatus { get }
var kAudioComponentErr_InvalidFormat: OSStatus { get }
typealias AudioUnitPropertyID = UInt32
typealias AudioUnitScope = UInt32
typealias AudioUnitElement = UInt32
typealias AudioUnitParameterID = UInt32
typealias AudioUnitParameterValue = Float32
enum AUParameterEventType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case parameterEvent_Immediate
  case parameterEvent_Ramped
}
struct AudioUnitParameterEvent {
  struct __Unnamed_union_eventValues {
    struct __Unnamed_struct_ramp {
      var startBufferOffset: Int32
      var durationInFrames: UInt32
      var startValue: AudioUnitParameterValue
      var endValue: AudioUnitParameterValue
      init()
      init(startBufferOffset startBufferOffset: Int32, durationInFrames durationInFrames: UInt32, startValue startValue: AudioUnitParameterValue, endValue endValue: AudioUnitParameterValue)
    }
    struct __Unnamed_struct_immediate {
      var bufferOffset: UInt32
      var value: AudioUnitParameterValue
      init()
      init(bufferOffset bufferOffset: UInt32, value value: AudioUnitParameterValue)
    }
    var ramp: AudioUnitParameterEvent.__Unnamed_union_eventValues.__Unnamed_struct_ramp
    var immediate: AudioUnitParameterEvent.__Unnamed_union_eventValues.__Unnamed_struct_immediate
    init(ramp ramp: AudioUnitParameterEvent.__Unnamed_union_eventValues.__Unnamed_struct_ramp)
    init(immediate immediate: AudioUnitParameterEvent.__Unnamed_union_eventValues.__Unnamed_struct_immediate)
    init()
  }
  var scope: AudioUnitScope
  var element: AudioUnitElement
  var parameter: AudioUnitParameterID
  var eventType: AUParameterEventType
  var eventValues: AudioUnitParameterEvent.__Unnamed_union_eventValues
  init()
  init(scope scope: AudioUnitScope, element element: AudioUnitElement, parameter parameter: AudioUnitParameterID, eventType eventType: AUParameterEventType, eventValues eventValues: AudioUnitParameterEvent.__Unnamed_union_eventValues)
}
struct AudioUnitParameter {
  var mAudioUnit: AudioUnit
  var mParameterID: AudioUnitParameterID
  var mScope: AudioUnitScope
  var mElement: AudioUnitElement
}
struct AudioUnitProperty {
  var mAudioUnit: AudioUnit
  var mPropertyID: AudioUnitPropertyID
  var mScope: AudioUnitScope
  var mElement: AudioUnitElement
}
typealias AURenderCallback = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<AudioUnitRenderActionFlags>, UnsafePointer<AudioTimeStamp>, UInt32, UInt32, UnsafeMutablePointer<AudioBufferList>?) -> OSStatus
typealias AudioUnitPropertyListenerProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnit, AudioUnitPropertyID, AudioUnitScope, AudioUnitElement) -> Void
typealias AUInputSamplesInOutputCallback = @convention(c) (UnsafeMutablePointer<Void>, UnsafePointer<AudioTimeStamp>, Float64, Float64) -> Void
@available(iOS 7.0, *)
let kAudioComponentRegistrationsChangedNotification: CFString
@available(iOS 9.0, *)
let kAudioComponentInstanceInvalidationNotification: CFString
@available(iOS 2.0, *)
@discardableResult
func AudioUnitInitialize(_ inUnit: AudioUnit) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitUninitialize(_ inUnit: AudioUnit) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitGetPropertyInfo(_ inUnit: AudioUnit, _ inID: AudioUnitPropertyID, _ inScope: AudioUnitScope, _ inElement: AudioUnitElement, _ outDataSize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitGetProperty(_ inUnit: AudioUnit, _ inID: AudioUnitPropertyID, _ inScope: AudioUnitScope, _ inElement: AudioUnitElement, _ outData: UnsafeMutablePointer<Void>, _ ioDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitSetProperty(_ inUnit: AudioUnit, _ inID: AudioUnitPropertyID, _ inScope: AudioUnitScope, _ inElement: AudioUnitElement, _ inData: UnsafePointer<Void>?, _ inDataSize: UInt32) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitAddPropertyListener(_ inUnit: AudioUnit, _ inID: AudioUnitPropertyID, _ inProc: AudioUnitPropertyListenerProc, _ inProcUserData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitRemovePropertyListenerWithUserData(_ inUnit: AudioUnit, _ inID: AudioUnitPropertyID, _ inProc: AudioUnitPropertyListenerProc, _ inProcUserData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitAddRenderNotify(_ inUnit: AudioUnit, _ inProc: AURenderCallback, _ inProcUserData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitRemoveRenderNotify(_ inUnit: AudioUnit, _ inProc: AURenderCallback, _ inProcUserData: UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitGetParameter(_ inUnit: AudioUnit, _ inID: AudioUnitParameterID, _ inScope: AudioUnitScope, _ inElement: AudioUnitElement, _ outValue: UnsafeMutablePointer<AudioUnitParameterValue>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitSetParameter(_ inUnit: AudioUnit, _ inID: AudioUnitParameterID, _ inScope: AudioUnitScope, _ inElement: AudioUnitElement, _ inValue: AudioUnitParameterValue, _ inBufferOffsetInFrames: UInt32) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitScheduleParameters(_ inUnit: AudioUnit, _ inParameterEvent: UnsafePointer<AudioUnitParameterEvent>, _ inNumParamEvents: UInt32) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitRender(_ inUnit: AudioUnit, _ ioActionFlags: UnsafeMutablePointer<AudioUnitRenderActionFlags>?, _ inTimeStamp: UnsafePointer<AudioTimeStamp>, _ inOutputBusNumber: UInt32, _ inNumberFrames: UInt32, _ ioData: UnsafeMutablePointer<AudioBufferList>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func AudioUnitProcess(_ inUnit: AudioUnit, _ ioActionFlags: UnsafeMutablePointer<AudioUnitRenderActionFlags>?, _ inTimeStamp: UnsafePointer<AudioTimeStamp>, _ inNumberFrames: UInt32, _ ioData: UnsafeMutablePointer<AudioBufferList>) -> OSStatus
@available(iOS 6.0, *)
@discardableResult
func AudioUnitProcessMultiple(_ inUnit: AudioUnit, _ ioActionFlags: UnsafeMutablePointer<AudioUnitRenderActionFlags>?, _ inTimeStamp: UnsafePointer<AudioTimeStamp>, _ inNumberFrames: UInt32, _ inNumberInputBufferLists: UInt32, _ inInputBufferLists: UnsafeMutablePointer<UnsafePointer<AudioBufferList>>, _ inNumberOutputBufferLists: UInt32, _ ioOutputBufferLists: UnsafeMutablePointer<UnsafeMutablePointer<AudioBufferList>>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioUnitReset(_ inUnit: AudioUnit, _ inScope: AudioUnitScope, _ inElement: AudioUnitElement) -> OSStatus
@available(iOS 7.0, *)
@discardableResult
func AudioOutputUnitPublish(_ inDesc: UnsafePointer<AudioComponentDescription>, _ inName: CFString, _ inVersion: UInt32, _ inOutputUnit: AudioUnit) -> OSStatus
@available(iOS 7.0, *)
@discardableResult
func AudioComponentGetLastActiveTime(_ comp: AudioComponent) -> CFAbsoluteTime
var kAudioUnitRange: Int { get }
var kAudioUnitInitializeSelect: Int { get }
var kAudioUnitUninitializeSelect: Int { get }
var kAudioUnitGetPropertyInfoSelect: Int { get }
var kAudioUnitGetPropertySelect: Int { get }
var kAudioUnitSetPropertySelect: Int { get }
var kAudioUnitAddPropertyListenerSelect: Int { get }
var kAudioUnitRemovePropertyListenerSelect: Int { get }
var kAudioUnitRemovePropertyListenerWithUserDataSelect: Int { get }
var kAudioUnitAddRenderNotifySelect: Int { get }
var kAudioUnitRemoveRenderNotifySelect: Int { get }
var kAudioUnitGetParameterSelect: Int { get }
var kAudioUnitSetParameterSelect: Int { get }
var kAudioUnitScheduleParametersSelect: Int { get }
var kAudioUnitRenderSelect: Int { get }
var kAudioUnitResetSelect: Int { get }
var kAudioUnitComplexRenderSelect: Int { get }
var kAudioUnitProcessSelect: Int { get }
var kAudioUnitProcessMultipleSelect: Int { get }
typealias AudioUnitInitializeProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioUnitUninitializeProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioUnitGetPropertyInfoProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitPropertyID, AudioUnitScope, AudioUnitElement, UnsafeMutablePointer<UInt32>?, UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
typealias AudioUnitGetPropertyProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitPropertyID, AudioUnitScope, AudioUnitElement, UnsafeMutablePointer<Void>, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioUnitSetPropertyProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitPropertyID, AudioUnitScope, AudioUnitElement, UnsafePointer<Void>, UInt32) -> OSStatus
typealias AudioUnitAddPropertyListenerProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitPropertyID, AudioUnitPropertyListenerProc, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioUnitRemovePropertyListenerProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitPropertyID, AudioUnitPropertyListenerProc) -> OSStatus
typealias AudioUnitRemovePropertyListenerWithUserDataProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitPropertyID, AudioUnitPropertyListenerProc, UnsafeMutablePointer<Void>?) -> OSStatus
typealias AudioUnitAddRenderNotifyProc = @convention(c) (UnsafeMutablePointer<Void>, AURenderCallback, UnsafeMutablePointer<Void>?) -> OSStatus
typealias AudioUnitRemoveRenderNotifyProc = @convention(c) (UnsafeMutablePointer<Void>, AURenderCallback, UnsafeMutablePointer<Void>?) -> OSStatus
typealias AudioUnitScheduleParametersProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafePointer<AudioUnitParameterEvent>, UInt32) -> OSStatus
typealias AudioUnitResetProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitScope, AudioUnitElement) -> OSStatus
typealias AudioUnitComplexRenderProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<AudioUnitRenderActionFlags>?, UnsafePointer<AudioTimeStamp>, UInt32, UInt32, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>, UnsafeMutablePointer<AudioBufferList>, UnsafeMutablePointer<Void>, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioUnitProcessProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<AudioUnitRenderActionFlags>?, UnsafePointer<AudioTimeStamp>, UInt32, UnsafeMutablePointer<AudioBufferList>) -> OSStatus
typealias AudioUnitProcessMultipleProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<AudioUnitRenderActionFlags>?, UnsafePointer<AudioTimeStamp>, UInt32, UInt32, UnsafeMutablePointer<UnsafePointer<AudioBufferList>>, UInt32, UnsafeMutablePointer<UnsafeMutablePointer<AudioBufferList>>) -> OSStatus
typealias AudioUnitGetParameterProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitParameterID, AudioUnitScope, AudioUnitElement, UnsafeMutablePointer<AudioUnitParameterValue>) -> OSStatus
typealias AudioUnitSetParameterProc = @convention(c) (UnsafeMutablePointer<Void>, AudioUnitParameterID, AudioUnitScope, AudioUnitElement, AudioUnitParameterValue, UInt32) -> OSStatus
typealias AudioUnitRenderProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<AudioUnitRenderActionFlags>?, UnsafePointer<AudioTimeStamp>, UInt32, UInt32, UnsafeMutablePointer<AudioBufferList>) -> OSStatus
var kAudioUnitErr_IllegalInstrument: OSStatus { get }
var kAudioUnitErr_InstrumentTypeNotFound: OSStatus { get }
