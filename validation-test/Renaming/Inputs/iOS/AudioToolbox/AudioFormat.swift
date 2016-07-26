
typealias AudioFormatPropertyID = UInt32
enum AudioPanningMode : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case panningMode_SoundField
  case panningMode_VectorBasedPanning
}
struct AudioPanningInfo {
  var mPanningMode: AudioPanningMode
  var mCoordinateFlags: UInt32
  var mCoordinates: (Float32, Float32, Float32)
  var mGainScale: Float32
  var mOutputChannelMap: UnsafePointer<AudioChannelLayout>
}
enum AudioBalanceFadeType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case maxUnityGain
  case equalPower
}
struct AudioBalanceFade {
  var mLeftRightBalance: Float32
  var mBackFrontFade: Float32
  var mType: AudioBalanceFadeType
  var mChannelLayout: UnsafePointer<AudioChannelLayout>
}
struct AudioFormatInfo {
  var mASBD: AudioStreamBasicDescription
  var mMagicCookie: UnsafePointer<Void>
  var mMagicCookieSize: UInt32
}
struct ExtendedAudioFormatInfo {
  var mASBD: AudioStreamBasicDescription
  var mMagicCookie: UnsafePointer<Void>
  var mMagicCookieSize: UInt32
  var mClassDescription: AudioClassDescription
}
struct AudioFormatListItem {
  var mASBD: AudioStreamBasicDescription
  var mChannelLayoutTag: AudioChannelLayoutTag
  init()
  init(mASBD mASBD: AudioStreamBasicDescription, mChannelLayoutTag mChannelLayoutTag: AudioChannelLayoutTag)
}
var kAudioFormatProperty_FormatInfo: AudioFormatPropertyID { get }
var kAudioFormatProperty_FormatName: AudioFormatPropertyID { get }
var kAudioFormatProperty_EncodeFormatIDs: AudioFormatPropertyID { get }
var kAudioFormatProperty_DecodeFormatIDs: AudioFormatPropertyID { get }
var kAudioFormatProperty_FormatList: AudioFormatPropertyID { get }
var kAudioFormatProperty_ASBDFromESDS: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelLayoutFromESDS: AudioFormatPropertyID { get }
var kAudioFormatProperty_OutputFormatList: AudioFormatPropertyID { get }
var kAudioFormatProperty_FirstPlayableFormatFromList: AudioFormatPropertyID { get }
var kAudioFormatProperty_FormatIsVBR: AudioFormatPropertyID { get }
var kAudioFormatProperty_FormatIsExternallyFramed: AudioFormatPropertyID { get }
var kAudioFormatProperty_FormatIsEncrypted: AudioFormatPropertyID { get }
var kAudioFormatProperty_Encoders: AudioFormatPropertyID { get }
var kAudioFormatProperty_Decoders: AudioFormatPropertyID { get }
var kAudioFormatProperty_AvailableEncodeBitRates: AudioFormatPropertyID { get }
var kAudioFormatProperty_AvailableEncodeSampleRates: AudioFormatPropertyID { get }
var kAudioFormatProperty_AvailableEncodeChannelLayoutTags: AudioFormatPropertyID { get }
var kAudioFormatProperty_AvailableEncodeNumberChannels: AudioFormatPropertyID { get }
var kAudioFormatProperty_ASBDFromMPEGPacket: AudioFormatPropertyID { get }
var kAudioFormatProperty_BitmapForLayoutTag: AudioFormatPropertyID { get }
var kAudioFormatProperty_MatrixMixMap: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelMap: AudioFormatPropertyID { get }
var kAudioFormatProperty_NumberOfChannelsForLayout: AudioFormatPropertyID { get }
var kAudioFormatProperty_AreChannelLayoutsEquivalent: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelLayoutHash: AudioFormatPropertyID { get }
var kAudioFormatProperty_ValidateChannelLayout: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelLayoutForTag: AudioFormatPropertyID { get }
var kAudioFormatProperty_TagForChannelLayout: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelLayoutName: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelLayoutSimpleName: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelLayoutForBitmap: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelName: AudioFormatPropertyID { get }
var kAudioFormatProperty_ChannelShortName: AudioFormatPropertyID { get }
var kAudioFormatProperty_TagsForNumberOfChannels: AudioFormatPropertyID { get }
var kAudioFormatProperty_PanningMatrix: AudioFormatPropertyID { get }
var kAudioFormatProperty_BalanceFade: AudioFormatPropertyID { get }
var kAudioFormatProperty_ID3TagSize: AudioFormatPropertyID { get }
var kAudioFormatProperty_ID3TagToDictionary: AudioFormatPropertyID { get }
var kAudioFormatProperty_HardwareCodecCapabilities: AudioFormatPropertyID { get }
var kAudioDecoderComponentType: UInt32 { get }
var kAudioEncoderComponentType: UInt32 { get }
var kAppleSoftwareAudioCodecManufacturer: UInt32 { get }
var kAppleHardwareAudioCodecManufacturer: UInt32 { get }
@available(iOS 2.0, *)
@discardableResult
func AudioFormatGetPropertyInfo(_ inPropertyID: AudioFormatPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ outPropertyDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioFormatGetProperty(_ inPropertyID: AudioFormatPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>?, _ outPropertyData: UnsafeMutablePointer<Void>?) -> OSStatus
var kAudioFormatUnspecifiedError: OSStatus { get }
var kAudioFormatUnsupportedPropertyError: OSStatus { get }
var kAudioFormatBadPropertySizeError: OSStatus { get }
var kAudioFormatBadSpecifierSizeError: OSStatus { get }
var kAudioFormatUnsupportedDataFormatError: OSStatus { get }
var kAudioFormatUnknownFormatError: OSStatus { get }
