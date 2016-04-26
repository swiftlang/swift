
typealias AudioCodec = AudioComponentInstance
typealias AudioCodecPropertyID = UInt32
struct AudioCodecMagicCookieInfo {
  var mMagicCookieSize: UInt32
  var mMagicCookie: UnsafePointer<Void>?
  init()
  init(mMagicCookieSize mMagicCookieSize: UInt32, mMagicCookie mMagicCookie: UnsafePointer<Void>?)
}
var kAudioDecoderComponentType: UInt32 { get }
var kAudioEncoderComponentType: UInt32 { get }
var kAudioUnityCodecComponentType: UInt32 { get }
var kAudioCodecPropertySupportedInputFormats: AudioCodecPropertyID { get }
var kAudioCodecPropertySupportedOutputFormats: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableInputSampleRates: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableOutputSampleRates: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableBitRateRange: AudioCodecPropertyID { get }
var kAudioCodecPropertyMinimumNumberInputPackets: AudioCodecPropertyID { get }
var kAudioCodecPropertyMinimumNumberOutputPackets: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableNumberChannels: AudioCodecPropertyID { get }
var kAudioCodecPropertyDoesSampleRateConversion: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableInputChannelLayoutTags: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableOutputChannelLayoutTags: AudioCodecPropertyID { get }
var kAudioCodecPropertyInputFormatsForOutputFormat: AudioCodecPropertyID { get }
var kAudioCodecPropertyOutputFormatsForInputFormat: AudioCodecPropertyID { get }
var kAudioCodecPropertyFormatInfo: AudioCodecPropertyID { get }
var kAudioCodecPropertyInputBufferSize: AudioCodecPropertyID { get }
var kAudioCodecPropertyPacketFrameSize: AudioCodecPropertyID { get }
var kAudioCodecPropertyHasVariablePacketByteSizes: AudioCodecPropertyID { get }
var kAudioCodecPropertyMaximumPacketByteSize: AudioCodecPropertyID { get }
var kAudioCodecPropertyPacketSizeLimitForVBR: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentInputFormat: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentOutputFormat: AudioCodecPropertyID { get }
var kAudioCodecPropertyMagicCookie: AudioCodecPropertyID { get }
var kAudioCodecPropertyUsedInputBufferSize: AudioCodecPropertyID { get }
var kAudioCodecPropertyIsInitialized: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentTargetBitRate: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentInputSampleRate: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentOutputSampleRate: AudioCodecPropertyID { get }
var kAudioCodecPropertyQualitySetting: AudioCodecPropertyID { get }
var kAudioCodecPropertyApplicableBitRateRange: AudioCodecPropertyID { get }
var kAudioCodecPropertyRecommendedBitRateRange: AudioCodecPropertyID { get }
var kAudioCodecPropertyApplicableInputSampleRates: AudioCodecPropertyID { get }
var kAudioCodecPropertyApplicableOutputSampleRates: AudioCodecPropertyID { get }
var kAudioCodecPropertyPaddedZeros: AudioCodecPropertyID { get }
var kAudioCodecPropertyPrimeMethod: AudioCodecPropertyID { get }
var kAudioCodecPropertyPrimeInfo: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentInputChannelLayout: AudioCodecPropertyID { get }
var kAudioCodecPropertyCurrentOutputChannelLayout: AudioCodecPropertyID { get }
var kAudioCodecPropertySettings: AudioCodecPropertyID { get }
var kAudioCodecPropertyFormatList: AudioCodecPropertyID { get }
var kAudioCodecPropertyBitRateControlMode: AudioCodecPropertyID { get }
var kAudioCodecPropertySoundQualityForVBR: AudioCodecPropertyID { get }
var kAudioCodecPropertyDelayMode: AudioCodecPropertyID { get }
var kAudioCodecPropertyAdjustLocalQuality: AudioCodecPropertyID { get }
var kAudioCodecPropertyProgramTargetLevel: AudioCodecPropertyID { get }
var kAudioCodecPropertyDynamicRangeControlMode: AudioCodecPropertyID { get }
var kAudioCodecPropertyProgramTargetLevelConstant: AudioCodecPropertyID { get }
var kAudioCodecQuality_Max: UInt32 { get }
var kAudioCodecQuality_High: UInt32 { get }
var kAudioCodecQuality_Medium: UInt32 { get }
var kAudioCodecQuality_Low: UInt32 { get }
var kAudioCodecQuality_Min: UInt32 { get }
var kAudioCodecPrimeMethod_Pre: UInt32 { get }
var kAudioCodecPrimeMethod_Normal: UInt32 { get }
var kAudioCodecPrimeMethod_None: UInt32 { get }
var kAudioCodecBitRateControlMode_Constant: UInt32 { get }
var kAudioCodecBitRateControlMode_LongTermAverage: UInt32 { get }
var kAudioCodecBitRateControlMode_VariableConstrained: UInt32 { get }
var kAudioCodecBitRateControlMode_Variable: UInt32 { get }
var kAudioCodecDelayMode_Compatibility: UInt32 { get }
var kAudioCodecDelayMode_Minimum: UInt32 { get }
var kAudioCodecDelayMode_Optimal: UInt32 { get }
var kProgramTargetLevel_None: UInt32 { get }
var kProgramTargetLevel_Minus31dB: UInt32 { get }
var kProgramTargetLevel_Minus23dB: UInt32 { get }
var kProgramTargetLevel_Minus20dB: UInt32 { get }
var kDynamicRangeControlMode_None: UInt32 { get }
var kDynamicRangeControlMode_Light: UInt32 { get }
var kDynamicRangeControlMode_Heavy: UInt32 { get }
struct AudioCodecPrimeInfo {
  var leadingFrames: UInt32
  var trailingFrames: UInt32
  init()
  init(leadingFrames leadingFrames: UInt32, trailingFrames trailingFrames: UInt32)
}
var kAudioSettings_TopLevelKey: String { get }
var kAudioSettings_Version: String { get }
var kAudioSettings_Parameters: String { get }
var kAudioSettings_SettingKey: String { get }
var kAudioSettings_SettingName: String { get }
var kAudioSettings_ValueType: String { get }
var kAudioSettings_AvailableValues: String { get }
var kAudioSettings_LimitedValues: String { get }
var kAudioSettings_CurrentValue: String { get }
var kAudioSettings_Summary: String { get }
var kAudioSettings_Hint: String { get }
var kAudioSettings_Unit: String { get }
struct AudioSettingsFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var expertParameter: AudioSettingsFlags { get }
  static var invisibleParameter: AudioSettingsFlags { get }
  static var metaParameter: AudioSettingsFlags { get }
  static var userInterfaceParameter: AudioSettingsFlags { get }
}
var kAudioCodecProduceOutputPacketFailure: UInt32 { get }
var kAudioCodecProduceOutputPacketSuccess: UInt32 { get }
var kAudioCodecProduceOutputPacketSuccessHasMore: UInt32 { get }
var kAudioCodecProduceOutputPacketNeedsMoreInputData: UInt32 { get }
var kAudioCodecProduceOutputPacketAtEOF: UInt32 { get }
var kAudioCodecGetPropertyInfoSelect: UInt32 { get }
var kAudioCodecGetPropertySelect: UInt32 { get }
var kAudioCodecSetPropertySelect: UInt32 { get }
var kAudioCodecInitializeSelect: UInt32 { get }
var kAudioCodecUninitializeSelect: UInt32 { get }
var kAudioCodecAppendInputDataSelect: UInt32 { get }
var kAudioCodecProduceOutputDataSelect: UInt32 { get }
var kAudioCodecResetSelect: UInt32 { get }
var kAudioCodecAppendInputBufferListSelect: UInt32 { get }
var kAudioCodecProduceOutputBufferListSelect: UInt32 { get }
var kAudioCodecNoError: OSStatus { get }
var kAudioCodecUnspecifiedError: OSStatus { get }
var kAudioCodecUnknownPropertyError: OSStatus { get }
var kAudioCodecBadPropertySizeError: OSStatus { get }
var kAudioCodecIllegalOperationError: OSStatus { get }
var kAudioCodecUnsupportedFormatError: OSStatus { get }
var kAudioCodecStateError: OSStatus { get }
var kAudioCodecNotEnoughBufferSpaceError: OSStatus { get }
@available(OSX 10.2, *)
@discardableResult
func AudioCodecGetPropertyInfo(_ inCodec: AudioCodec, _ inPropertyID: AudioCodecPropertyID, _ outSize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecGetProperty(_ inCodec: AudioCodec, _ inPropertyID: AudioCodecPropertyID, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecSetProperty(_ inCodec: AudioCodec, _ inPropertyID: AudioCodecPropertyID, _ inPropertyDataSize: UInt32, _ inPropertyData: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecInitialize(_ inCodec: AudioCodec, _ inInputFormat: UnsafePointer<AudioStreamBasicDescription>?, _ inOutputFormat: UnsafePointer<AudioStreamBasicDescription>?, _ inMagicCookie: UnsafePointer<Void>?, _ inMagicCookieByteSize: UInt32) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecUninitialize(_ inCodec: AudioCodec) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecAppendInputData(_ inCodec: AudioCodec, _ inInputData: UnsafePointer<Void>, _ ioInputDataByteSize: UnsafeMutablePointer<UInt32>, _ ioNumberPackets: UnsafeMutablePointer<UInt32>, _ inPacketDescription: UnsafePointer<AudioStreamPacketDescription>?) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecProduceOutputPackets(_ inCodec: AudioCodec, _ outOutputData: UnsafeMutablePointer<Void>, _ ioOutputDataByteSize: UnsafeMutablePointer<UInt32>, _ ioNumberPackets: UnsafeMutablePointer<UInt32>, _ outPacketDescription: UnsafeMutablePointer<AudioStreamPacketDescription>?, _ outStatus: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func AudioCodecAppendInputBufferList(_ inCodec: AudioCodec, _ inBufferList: UnsafePointer<AudioBufferList>, _ ioNumberPackets: UnsafeMutablePointer<UInt32>, _ inPacketDescription: UnsafePointer<AudioStreamPacketDescription>?, _ outBytesConsumed: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func AudioCodecProduceOutputBufferList(_ inCodec: AudioCodec, _ ioBufferList: UnsafeMutablePointer<AudioBufferList>, _ ioNumberPackets: UnsafeMutablePointer<UInt32>, _ outPacketDescription: UnsafeMutablePointer<AudioStreamPacketDescription>?, _ outStatus: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AudioCodecReset(_ inCodec: AudioCodec) -> OSStatus
typealias AudioCodecGetPropertyInfoProc = @convention(c) (UnsafeMutablePointer<Void>, AudioCodecPropertyID, UnsafeMutablePointer<UInt32>?, UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
typealias AudioCodecGetPropertyProc = @convention(c) (UnsafeMutablePointer<Void>, AudioCodecPropertyID, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioCodecSetPropertyProc = @convention(c) (UnsafeMutablePointer<Void>, AudioCodecPropertyID, UInt32, UnsafePointer<Void>) -> OSStatus
typealias AudioCodecInitializeProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafePointer<AudioStreamBasicDescription>?, UnsafePointer<AudioStreamBasicDescription>?, UnsafePointer<Void>?, UInt32) -> OSStatus
typealias AudioCodecUninitializeProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioCodecAppendInputDataProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafePointer<Void>, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<UInt32>, UnsafePointer<AudioStreamPacketDescription>?) -> OSStatus
typealias AudioCodecProduceOutputPacketsProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<Void>, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>?, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioCodecResetProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioCodecAppendInputBufferListProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafePointer<AudioBufferList>, UnsafeMutablePointer<UInt32>, UnsafePointer<AudioStreamPacketDescription>?, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioCodecProduceOutputBufferListProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<AudioBufferList>, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>?, UnsafeMutablePointer<UInt32>) -> OSStatus
var kAudioCodecPropertyMinimumDelayMode: AudioCodecPropertyID { get }
var kAudioCodecPropertyNameCFString: AudioCodecPropertyID { get }
var kAudioCodecPropertyManufacturerCFString: AudioCodecPropertyID { get }
var kAudioCodecPropertyFormatCFString: AudioCodecPropertyID { get }
var kAudioCodecPropertyRequiresPacketDescription: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableBitRates: AudioCodecPropertyID { get }
var kAudioCodecExtendFrequencies: AudioCodecPropertyID { get }
var kAudioCodecUseRecommendedSampleRate: AudioCodecPropertyID { get }
var kAudioCodecOutputPrecedence: AudioCodecPropertyID { get }
var kAudioCodecBitRateFormat: AudioCodecPropertyID { get }
var kAudioCodecDoesSampleRateConversion: AudioCodecPropertyID { get }
var kAudioCodecInputFormatsForOutputFormat: AudioCodecPropertyID { get }
var kAudioCodecOutputFormatsForInputFormat: AudioCodecPropertyID { get }
var kAudioCodecPropertyInputChannelLayout: AudioCodecPropertyID { get }
var kAudioCodecPropertyOutputChannelLayout: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableInputChannelLayouts: AudioCodecPropertyID { get }
var kAudioCodecPropertyAvailableOutputChannelLayouts: AudioCodecPropertyID { get }
var kAudioCodecPropertyZeroFramesPadded: AudioCodecPropertyID { get }
var kAudioCodecBitRateFormat_CBR: UInt32 { get }
var kAudioCodecBitRateFormat_ABR: UInt32 { get }
var kAudioCodecBitRateFormat_VBR: UInt32 { get }
var kAudioCodecOutputPrecedenceNone: UInt32 { get }
var kAudioCodecOutputPrecedenceBitRate: UInt32 { get }
var kAudioCodecOutputPrecedenceSampleRate: UInt32 { get }
@available(*, deprecated)
typealias MagicCookieInfo = AudioCodecMagicCookieInfo
var kHintBasic: UInt32 { get }
var kHintAdvanced: UInt32 { get }
var kHintHidden: UInt32 { get }
