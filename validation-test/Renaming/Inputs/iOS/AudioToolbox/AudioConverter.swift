
typealias AudioConverterRef = OpaquePointer
typealias AudioConverterPropertyID = UInt32
var kAudioConverterPropertyMinimumInputBufferSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyMinimumOutputBufferSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyMaximumInputBufferSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyMaximumInputPacketSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyMaximumOutputPacketSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyCalculateInputBufferSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyCalculateOutputBufferSize: AudioConverterPropertyID { get }
var kAudioConverterPropertyInputCodecParameters: AudioConverterPropertyID { get }
var kAudioConverterPropertyOutputCodecParameters: AudioConverterPropertyID { get }
var kAudioConverterSampleRateConverterAlgorithm: AudioConverterPropertyID { get }
var kAudioConverterSampleRateConverterComplexity: AudioConverterPropertyID { get }
var kAudioConverterSampleRateConverterQuality: AudioConverterPropertyID { get }
var kAudioConverterSampleRateConverterInitialPhase: AudioConverterPropertyID { get }
var kAudioConverterCodecQuality: AudioConverterPropertyID { get }
var kAudioConverterPrimeMethod: AudioConverterPropertyID { get }
var kAudioConverterPrimeInfo: AudioConverterPropertyID { get }
var kAudioConverterChannelMap: AudioConverterPropertyID { get }
var kAudioConverterDecompressionMagicCookie: AudioConverterPropertyID { get }
var kAudioConverterCompressionMagicCookie: AudioConverterPropertyID { get }
var kAudioConverterEncodeBitRate: AudioConverterPropertyID { get }
var kAudioConverterEncodeAdjustableSampleRate: AudioConverterPropertyID { get }
var kAudioConverterInputChannelLayout: AudioConverterPropertyID { get }
var kAudioConverterOutputChannelLayout: AudioConverterPropertyID { get }
var kAudioConverterApplicableEncodeBitRates: AudioConverterPropertyID { get }
var kAudioConverterAvailableEncodeBitRates: AudioConverterPropertyID { get }
var kAudioConverterApplicableEncodeSampleRates: AudioConverterPropertyID { get }
var kAudioConverterAvailableEncodeSampleRates: AudioConverterPropertyID { get }
var kAudioConverterAvailableEncodeChannelLayoutTags: AudioConverterPropertyID { get }
var kAudioConverterCurrentOutputStreamDescription: AudioConverterPropertyID { get }
var kAudioConverterCurrentInputStreamDescription: AudioConverterPropertyID { get }
var kAudioConverterPropertySettings: AudioConverterPropertyID { get }
var kAudioConverterPropertyBitDepthHint: AudioConverterPropertyID { get }
var kAudioConverterPropertyFormatList: AudioConverterPropertyID { get }
var kAudioConverterPropertyCanResumeFromInterruption: AudioConverterPropertyID { get }
var kAudioConverterQuality_Max: UInt32 { get }
var kAudioConverterQuality_High: UInt32 { get }
var kAudioConverterQuality_Medium: UInt32 { get }
var kAudioConverterQuality_Low: UInt32 { get }
var kAudioConverterQuality_Min: UInt32 { get }
var kAudioConverterSampleRateConverterComplexity_Linear: UInt32 { get }
var kAudioConverterSampleRateConverterComplexity_Normal: UInt32 { get }
var kAudioConverterSampleRateConverterComplexity_Mastering: UInt32 { get }
var kConverterPrimeMethod_Pre: UInt32 { get }
var kConverterPrimeMethod_Normal: UInt32 { get }
var kConverterPrimeMethod_None: UInt32 { get }
struct AudioConverterPrimeInfo {
  var leadingFrames: UInt32
  var trailingFrames: UInt32
  init()
  init(leadingFrames leadingFrames: UInt32, trailingFrames trailingFrames: UInt32)
}
var kAudioConverterErr_FormatNotSupported: OSStatus { get }
var kAudioConverterErr_OperationNotSupported: OSStatus { get }
var kAudioConverterErr_PropertyNotSupported: OSStatus { get }
var kAudioConverterErr_InvalidInputSize: OSStatus { get }
var kAudioConverterErr_InvalidOutputSize: OSStatus { get }
var kAudioConverterErr_UnspecifiedError: OSStatus { get }
var kAudioConverterErr_BadPropertySizeError: OSStatus { get }
var kAudioConverterErr_RequiresPacketDescriptionsError: OSStatus { get }
var kAudioConverterErr_InputSampleRateOutOfRange: OSStatus { get }
var kAudioConverterErr_OutputSampleRateOutOfRange: OSStatus { get }
var kAudioConverterErr_HardwareInUse: OSStatus { get }
var kAudioConverterErr_NoHardwarePermission: OSStatus { get }
@available(iOS 2.0, *)
@discardableResult
func AudioConverterNew(_ inSourceFormat: UnsafePointer<AudioStreamBasicDescription>, _ inDestinationFormat: UnsafePointer<AudioStreamBasicDescription>, _ outAudioConverter: UnsafeMutablePointer<AudioConverterRef?>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterNewSpecific(_ inSourceFormat: UnsafePointer<AudioStreamBasicDescription>, _ inDestinationFormat: UnsafePointer<AudioStreamBasicDescription>, _ inNumberClassDescriptions: UInt32, _ inClassDescriptions: UnsafePointer<AudioClassDescription>, _ outAudioConverter: UnsafeMutablePointer<AudioConverterRef?>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterDispose(_ inAudioConverter: AudioConverterRef) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterReset(_ inAudioConverter: AudioConverterRef) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterGetPropertyInfo(_ inAudioConverter: AudioConverterRef, _ inPropertyID: AudioConverterPropertyID, _ outSize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterGetProperty(_ inAudioConverter: AudioConverterRef, _ inPropertyID: AudioConverterPropertyID, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterSetProperty(_ inAudioConverter: AudioConverterRef, _ inPropertyID: AudioConverterPropertyID, _ inPropertyDataSize: UInt32, _ inPropertyData: UnsafePointer<Void>) -> OSStatus
typealias AudioConverterInputDataProc = @convention(c) (AudioConverterRef, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<UnsafeMutablePointer<Void>>, UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterConvertBuffer(_ inAudioConverter: AudioConverterRef, _ inInputDataSize: UInt32, _ inInputData: UnsafePointer<Void>, _ ioOutputDataSize: UnsafeMutablePointer<UInt32>, _ outOutputData: UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioConverterComplexInputDataProc = @convention(c) (AudioConverterRef, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioBufferList>, UnsafeMutablePointer<UnsafeMutablePointer<AudioStreamPacketDescription>?>?, UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioConverterFillComplexBuffer(_ inAudioConverter: AudioConverterRef, _ inInputDataProc: AudioConverterComplexInputDataProc, _ inInputDataProcUserData: UnsafeMutablePointer<Void>?, _ ioOutputDataPacketSize: UnsafeMutablePointer<UInt32>, _ outOutputData: UnsafeMutablePointer<AudioBufferList>, _ outPacketDescription: UnsafeMutablePointer<AudioStreamPacketDescription>?) -> OSStatus
@available(iOS 5.0, *)
@discardableResult
func AudioConverterConvertComplexBuffer(_ inAudioConverter: AudioConverterRef, _ inNumberPCMFrames: UInt32, _ inInputData: UnsafePointer<AudioBufferList>, _ outOutputData: UnsafeMutablePointer<AudioBufferList>) -> OSStatus
