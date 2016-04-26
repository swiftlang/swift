
var kDVDErrorUnknown: Int { get }
var kDVDErrorInitializingLib: Int { get }
var kDVDErrorUninitializedLib: Int { get }
var kDVDErrorNotAllowedDuringPlayback: Int { get }
var kDVDErrorUnassignedGrafPort: Int { get }
var kDVDErrorAlreadyPlaying: Int { get }
var kDVDErrorNoFatalErrCallBack: Int { get }
var kDVDErrorIsAlreadySleeping: Int { get }
var kDVDErrorDontNeedWakeup: Int { get }
var kDVDErrorTimeOutOfRange: Int { get }
var kDVDErrorUserActionNoOp: Int { get }
var kDVDErrorMissingDrive: Int { get }
var kDVDErrorNotSupportedConfiguration: Int { get }
var kDVDErrorNotSupportedFunction: Int { get }
var kDVDErrorNoValidMedia: Int { get }
var kDVDErrorWrongParam: Int { get }
var kDVDErrorMissingGraphicsDevice: Int { get }
var kDVDErrorGraphicsDevice: Int { get }
var kDVDErrorPlaybackOpen: Int { get }
var kDVDErrorInvalidRegionCode: Int { get }
var kDVDErrorRgnMgrInstall: Int { get }
var kDVDErrorMismatchedRegionCode: Int { get }
var kDVDErrorNoMoreRegionSets: Int { get }
var kDVDErrordRegionCodeUninitialized: Int { get }
var kDVDErrorAuthentification: Int { get }
var kDVDErrorOutOfVideoMemory: Int { get }
var kDVDErrorNoAudioOutputDevice: Int { get }
var kDVDErrorSystem: Int { get }
var kDVDErrorNavigation: Int { get }
var kDVDErrorInvalidBookmarkVersion: Int { get }
var kDVDErrorInvalidBookmarkSize: Int { get }
var kDVDErrorInvalidBookmarkForMedia: Int { get }
var kDVDErrorNoValidBookmarkForLastPlay: Int { get }
var kDVDErrorDisplayAuthentification: Int { get }
typealias DVDErrorCode = OSStatus
enum DVDState : OSStatus {
  init?(rawValue rawValue: OSStatus)
  var rawValue: OSStatus { get }
  case unknown
  case playing
  case playingStill
  case paused
  case stopped
  case scanning
  case idle
  case playingSlow
}
enum DVDMenu : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case title
  case root
  case subPicture
  case audio
  case angle
  case PTT
  case none
}
var kDVDButtonIndexNone: Int { get }
typealias DVDButtonIndex = Int32
enum DVDUserNavigation : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case moveUp
  case moveDown
  case moveLeft
  case moveRight
  case enter
}
typealias DVDTimePosition = UInt32
var kDVDTimeCodeUninitialized: Int { get }
var kDVDTimeCodeElapsedSeconds: Int { get }
var kDVDTimeCodeRemainingSeconds: Int { get }
var kDVDTimeCodeTitleDurationSeconds: Int { get }
var kDVDTimeCodeChapterElapsedSeconds: Int { get }
var kDVDTimeCodeChapterRemainingSeconds: Int { get }
var kDVDTimeCodeChapterDurationSeconds: Int { get }
typealias DVDTimeCode = Int16
enum DVDScanDirection : Int8 {
  init?(rawValue rawValue: Int8)
  var rawValue: Int8 { get }
  case forward
  case backward
}
enum DVDScanRate : Int16 {
  init?(rawValue rawValue: Int16)
  var rawValue: Int16 { get }
  case rateOneEigth
  case rateOneFourth
  case rateOneHalf
  case rate1x
  case rate2x
  case rate4x
  case rate8x
  case rate16x
  case rate32x
}
enum DVDAspectRatio : Int16 {
  init?(rawValue rawValue: Int16)
  var rawValue: Int16 { get }
  case ratioUninitialized
  case ratio4x3
  case ratio4x3PanAndScan
  case ratio16x9
  case ratioLetterBox
}
enum DVDFormat : Int16 {
  init?(rawValue rawValue: Int16)
  var rawValue: Int16 { get }
  case uninitialized
  case NTSC
  case PAL
  case NTSC_HDTV
  case PAL_HDTV
}
var kDVDAudioModeUninitialized: Int { get }
var kDVDAudioModeProLogic: Int { get }
var kDVDAudioModeSPDIF: Int { get }
typealias DVDAudioMode = Int32
enum DVDAudioFormat : Int16 {
  init?(rawValue rawValue: Int16)
  var rawValue: Int16 { get }
  case unknownFormat
  case ac3Format
  case mpeg1Format
  case mpeg2Format
  case pcmFormat
  case dtsFormat
  case sddsFormat
  case mlpFormat
  case ddPlusFormat
  case dtshdFormat
}
var kDVDLanguageCodeUninitialized: Int { get }
var kDVDLanguageNoPreference: Int { get }
var kDVDLanguageCodeNone: Int { get }
var kDVDLanguageCodeAfar: Int { get }
var kDVDLanguageCodeAbkhazian: Int { get }
var kDVDLanguageCodeAfrikaans: Int { get }
var kDVDLanguageCodeAmharic: Int { get }
var kDVDLanguageCodeArabic: Int { get }
var kDVDLanguageCodeAssamese: Int { get }
var kDVDLanguageCodeAymara: Int { get }
var kDVDLanguageCodeAzerbaijani: Int { get }
var kDVDLanguageCodeBashkir: Int { get }
var kDVDLanguageCodeByelorussian: Int { get }
var kDVDLanguageCodeBulgarian: Int { get }
var kDVDLanguageCodeBihari: Int { get }
var kDVDLanguageCodeBislama: Int { get }
var kDVDLanguageCodeBengali: Int { get }
var kDVDLanguageCodeTibetan: Int { get }
var kDVDLanguageCodeBreton: Int { get }
var kDVDLanguageCodeCatalan: Int { get }
var kDVDLanguageCodeCorsican: Int { get }
var kDVDLanguageCodeCzech: Int { get }
var kDVDLanguageCodeWelsh: Int { get }
var kDVDLanguageCodeDanish: Int { get }
var kDVDLanguageCodeGerman: Int { get }
var kDVDLanguageCodeBhutani: Int { get }
var kDVDLanguageCodeGreek: Int { get }
var kDVDLanguageCodeEnglish: Int { get }
var kDVDLanguageCodeEsperanto: Int { get }
var kDVDLanguageCodeSpanish: Int { get }
var kDVDLanguageCodeEstonian: Int { get }
var kDVDLanguageCodeBasque: Int { get }
var kDVDLanguageCodePersian: Int { get }
var kDVDLanguageCodeFinnish: Int { get }
var kDVDLanguageCodeFiji: Int { get }
var kDVDLanguageCodeFaeroese: Int { get }
var kDVDLanguageCodeFrench: Int { get }
var kDVDLanguageCodeFrisian: Int { get }
var kDVDLanguageCodeIrish: Int { get }
var kDVDLanguageCodeScotsGaelic: Int { get }
var kDVDLanguageCodeGalician: Int { get }
var kDVDLanguageCodeGuarani: Int { get }
var kDVDLanguageCodeGujarati: Int { get }
var kDVDLanguageCodeHausa: Int { get }
var kDVDLanguageCodeHindi: Int { get }
var kDVDLanguageCodeCroatian: Int { get }
var kDVDLanguageCodeHungarian: Int { get }
var kDVDLanguageCodeArmenian: Int { get }
var kDVDLanguageCodeInterlingua: Int { get }
var kDVDLanguageCodeInterlingue: Int { get }
var kDVDLanguageCodeInupiak: Int { get }
var kDVDLanguageCodeIndonesian: Int { get }
var kDVDLanguageCodeIcelandic: Int { get }
var kDVDLanguageCodeItalian: Int { get }
var kDVDLanguageCodeHebrew: Int { get }
var kDVDLanguageCodeJapanese: Int { get }
var kDVDLanguageCodeYiddish: Int { get }
var kDVDLanguageCodeJavanese: Int { get }
var kDVDLanguageCodeGeorgian: Int { get }
var kDVDLanguageCodeKazakh: Int { get }
var kDVDLanguageCodeGreenlandic: Int { get }
var kDVDLanguageCodeCambodian: Int { get }
var kDVDLanguageCodeKannada: Int { get }
var kDVDLanguageCodeKorean: Int { get }
var kDVDLanguageCodeKashmiri: Int { get }
var kDVDLanguageCodeKurdish: Int { get }
var kDVDLanguageCodeKirghiz: Int { get }
var kDVDLanguageCodeLatin: Int { get }
var kDVDLanguageCodeLingala: Int { get }
var kDVDLanguageCodeLaothian: Int { get }
var kDVDLanguageCodeLithuanian: Int { get }
var kDVDLanguageCodeLatvian: Int { get }
var kDVDLanguageCodeMalagasy: Int { get }
var kDVDLanguageCodeMaori: Int { get }
var kDVDLanguageCodeMacedonian: Int { get }
var kDVDLanguageCodeMalayalam: Int { get }
var kDVDLanguageCodeMongolian: Int { get }
var kDVDLanguageCodeMoldavian: Int { get }
var kDVDLanguageCodeMarathi: Int { get }
var kDVDLanguageCodeMalay: Int { get }
var kDVDLanguageCodeMaltese: Int { get }
var kDVDLanguageCodeBurmese: Int { get }
var kDVDLanguageCodeNauru: Int { get }
var kDVDLanguageCodeNepali: Int { get }
var kDVDLanguageCodeDutch: Int { get }
var kDVDLanguageCodeNorwegian: Int { get }
var kDVDLanguageCodeOccitan: Int { get }
var kDVDLanguageCodeOromo: Int { get }
var kDVDLanguageCodeOriya: Int { get }
var kDVDLanguageCodePunjabi: Int { get }
var kDVDLanguageCodePolish: Int { get }
var kDVDLanguageCodePashto: Int { get }
var kDVDLanguageCodePortugese: Int { get }
var kDVDLanguageCodeQuechua: Int { get }
var kDVDLanguageCodeRhaetoRomance: Int { get }
var kDVDLanguageCodeKirundi: Int { get }
var kDVDLanguageCodeRomanian: Int { get }
var kDVDLanguageCodeRussian: Int { get }
var kDVDLanguageCodeKinyarwanda: Int { get }
var kDVDLanguageCodeSanskrit: Int { get }
var kDVDLanguageCodeSindhi: Int { get }
var kDVDLanguageCodeSangro: Int { get }
var kDVDLanguageCodeSerboCroatian: Int { get }
var kDVDLanguageCodeSinghalese: Int { get }
var kDVDLanguageCodeSlovak: Int { get }
var kDVDLanguageCodeSlovenian: Int { get }
var kDVDLanguageCodeSamoan: Int { get }
var kDVDLanguageCodeShona: Int { get }
var kDVDLanguageCodeSomali: Int { get }
var kDVDLanguageCodeAlbanian: Int { get }
var kDVDLanguageCodeSerbian: Int { get }
var kDVDLanguageCodeSiswati: Int { get }
var kDVDLanguageCodeSesotho: Int { get }
var kDVDLanguageCodeSudanese: Int { get }
var kDVDLanguageCodeSwedish: Int { get }
var kDVDLanguageCodeSwahili: Int { get }
var kDVDLanguageCodeTamil: Int { get }
var kDVDLanguageCodeTelugu: Int { get }
var kDVDLanguageCodeTajik: Int { get }
var kDVDLanguageCodeThai: Int { get }
var kDVDLanguageCodeTigrinya: Int { get }
var kDVDLanguageCodeTurkmen: Int { get }
var kDVDLanguageCodeTagalog: Int { get }
var kDVDLanguageCodeSetswana: Int { get }
var kDVDLanguageCodeTonga: Int { get }
var kDVDLanguageCodeTurkish: Int { get }
var kDVDLanguageCodeTsonga: Int { get }
var kDVDLanguageCodeTatar: Int { get }
var kDVDLanguageCodeTwi: Int { get }
var kDVDLanguageCodeUkranian: Int { get }
var kDVDLanguageCodeUrdu: Int { get }
var kDVDLanguageCodeUzbek: Int { get }
var kDVDLanguageCodeVietnamese: Int { get }
var kDVDLanguageCodeVolapuk: Int { get }
var kDVDLanguageCodeWolof: Int { get }
var kDVDLanguageCodeXhosa: Int { get }
var kDVDLanguageCodeYoruba: Int { get }
var kDVDLanguageCodeChinese: Int { get }
var kDVDLanguageCodeZulu: Int { get }
typealias DVDLanguageCode = OSType
var kDVDAudioExtensionCodeNotSpecified: Int { get }
var kDVDAudioExtensionCodeNormalCaptions: Int { get }
var kDVDAudioExtensionCodeNVisualImpaired: Int { get }
var kDVDAudioExtensionCodeDirectorsComment1: Int { get }
var kDVDAudioExtensionCodeDirectorsComment2: Int { get }
typealias DVDAudioExtensionCode = OSType
var kDVDSubpictureExtensionCodeNotSpecified: Int { get }
var kDVDSubpictureExtensionCodeCaptionNormalSize: Int { get }
var kDVDSubpictureExtensionCodeCaptionBiggerSize: Int { get }
var kDVDSubpictureExtensionCodeCaption4Children: Int { get }
var kDVDSubpictureExtensionCodeClosedCaptionNormalSize: Int { get }
var kDVDSubpictureExtensionCodeClosedCaptionBiggerSize: Int { get }
var kDVDSubpictureExtensionCodeClosedCaption4Children: Int { get }
var kDVDSubpictureExtensionCodeForcedCaption: Int { get }
var kDVDSubpictureExtensionDirectorsCommentNormalSize: Int { get }
var kDVDSubpictureExtensionDirectorsCommentBiggerSize: Int { get }
var kDVDSubpictureExtensionDirectorsComment4Children: Int { get }
typealias DVDSubpictureExtensionCode = OSType
typealias DVDDiscID = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
var kDVDRegionCodeUninitialized: Int { get }
var kDVDRegionCode1: Int { get }
var kDVDRegionCode2: Int { get }
var kDVDRegionCode3: Int { get }
var kDVDRegionCode4: Int { get }
var kDVDRegionCode5: Int { get }
var kDVDRegionCode6: Int { get }
var kDVDRegionCode7: Int { get }
var kDVDRegionCode8: Int { get }
typealias DVDRegionCode = UInt32
enum DVDDomainCode : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case dvdfpDomain
  case dvdvmgmDomain
  case dvdvtsmDomain
  case dvdttDomain
  case dvdstopDomain
  case dvdamgmDomain
  case dvdttgrDomain
}
var kDVDUOPTimePlaySearch: Int { get }
var kDVDUOPPTTPlaySearch: Int { get }
var kDVDUOPTitlePlay: Int { get }
var kDVDUOPStop: Int { get }
var kDVDUOPGoUp: Int { get }
var kDVDUOPTimePTTSearch: Int { get }
var kDVDUOPPrevTopPGSearch: Int { get }
var kDVDUOPNextPGSearch: Int { get }
var kDVDUOPForwardScan: Int { get }
var kDVDUOPBackwardScan: Int { get }
var kDVDUOPMenuCallTitle: Int { get }
var kDVDUOPMenuCallRoot: Int { get }
var kDVDUOPMenuCallSubPicture: Int { get }
var kDVDUOPMenuCallAudio: Int { get }
var kDVDUOPMenuCallAngle: Int { get }
var kDVDUOPMenuCallPTT: Int { get }
var kDVDUOPResume: Int { get }
var kDVDUOPButton: Int { get }
var kDVDUOPStillOff: Int { get }
var kDVDUOPPauseOn: Int { get }
var kDVDUOPAudioStreamChange: Int { get }
var kDVDUOPSubPictureStreamChange: Int { get }
var kDVDUOPAngleChange: Int { get }
var kDVDUOPKaraokeModeChange: Int { get }
var kDVDUOPVideoModeChange: Int { get }
var kDVDUOPScanOff: Int { get }
var kDVDUOPPauseOff: Int { get }
typealias DVDUOPCode = UInt32
enum DVDEventCode : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case title
  case PTT
  case validUOP
  case angle
  case audioStream
  case subpictureStream
  case displayMode
  case domain
  case bitrate
  case still
  case playback
  case videoStandard
  case streams
  case scanSpeed
  case menuCalled
  case parental
  case PGC
  case GPRM
  case regionMismatch
  case titleTime
  case subpictureStreamNumbers
  case audioStreamNumbers
  case angleNumbers
  case error
  case ccInfo
  case chapterTime
}
typealias DVDEventValue = UInt
typealias DVDEventCallBackRef = UnsafeMutablePointer<Void>
typealias DVDFatalErrCallBackFunctionPtr = @convention(c) (DVDErrorCode, UnsafeMutablePointer<Void>) -> Void
typealias DVDEventCallBackFunctionPtr = @convention(c) (DVDEventCode, DVDEventValue, DVDEventValue, UnsafeMutablePointer<Void>) -> Void
@available(OSX 10.3, *)
@discardableResult
func DVDInitialize() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDDispose() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsValidMediaRef(_ inRef: UnsafeMutablePointer<FSRef>, _ outIsValid: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDIsValidMediaURL(_ inRef: CFURL, _ outIsValid: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDHasMedia(_ outHasMedia: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDOpenMediaFile(_ inFile: UnsafeMutablePointer<FSRef>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDOpenMediaFileWithURL(_ inFile: CFURL) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDCloseMediaFile() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDOpenMediaVolume(_ inVolume: UnsafeMutablePointer<FSRef>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDOpenMediaVolumeWithURL(_ inVolume: CFURL) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDCloseMediaVolume() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsSupportedDisplay(_ inDisplay: CGDirectDisplayID, _ outSupported: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSwitchToDisplay(_ newDisplay: CGDirectDisplayID, _ outSupported: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetVideoDisplay(_ inDisplay: CGDirectDisplayID) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetVideoDisplay(_ outDisplay: UnsafeMutablePointer<CGDirectDisplayID>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetVideoWindowID(_ inVidWindowID: UInt32) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetVideoWindowID(_ outVidWindowID: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetNativeVideoSize(_ outWidth: UnsafeMutablePointer<UInt16>, _ outHeight: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAspectRatio(_ outRatio: UnsafeMutablePointer<DVDAspectRatio>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetAspectRatio(_ inRatio: DVDAspectRatio) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetFormatStandard(_ outFormat: UnsafeMutablePointer<DVDFormat>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDSetVideoWindowRef(_ inWindowRef: WindowRef?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDGetVideoWindowRef(_ outWindowRef: UnsafeMutablePointer<WindowRef>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDSetVideoCGBounds(_ inRect: UnsafeMutablePointer<CGRect>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDGetVideoCGBounds(_ outRect: UnsafeMutablePointer<CGRect>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioStreamFormat(_ outFormat: UnsafeMutablePointer<DVDAudioFormat>?, _ outBitsPerSample: UnsafeMutablePointer<UInt32>?, _ outSamplesPerSecond: UnsafeMutablePointer<UInt32>?, _ outChannels: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func DVDGetAudioStreamFormatByStream(_ inStreamNum: UInt32, _ outFormat: UnsafeMutablePointer<DVDAudioFormat>?, _ outBitsPerSample: UnsafeMutablePointer<UInt32>?, _ outSamplesPerSecond: UnsafeMutablePointer<UInt32>?, _ outChannels: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioOutputModeCapabilities(_ outModes: UnsafeMutablePointer<DVDAudioMode>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetAudioOutputMode(_ inMode: DVDAudioMode) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioOutputMode(_ outMode: UnsafeMutablePointer<DVDAudioMode>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetSPDIFDataOutDeviceCount(_ outCount: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetSPDIFDataOutDeviceCFName(_ inIndex: UInt32, _ outName: UnsafeMutablePointer<Unmanaged<CFString>>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetSPDIFDataOutDevice(_ inIndex: UInt32) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetSPDIFDataOutDevice(_ outIndex: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetTime(_ inTimeCode: DVDTimeCode, _ inTime: DVDTimePosition, _ inFrames: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetTime(_ inTimeCode: DVDTimeCode, _ outTime: UnsafeMutablePointer<DVDTimePosition>, _ outFrames: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetState(_ outState: UnsafeMutablePointer<DVDState>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIdle() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDUpdateVideo() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsPlaying(_ outIsPlaying: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsPaused(_ outIsPaused: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDPlay() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDPause() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDResume() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDStop() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDScan(_ inRate: DVDScanRate, _ inDirection: DVDScanDirection) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetScanRate(_ outRate: UnsafeMutablePointer<DVDScanRate>?, _ outDirection: UnsafeMutablePointer<DVDScanDirection>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDStepFrame(_ inDirection: DVDScanDirection) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsMuted(_ outIsMuted: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDMute(_ inMute: Bool) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetAudioVolume(_ inVolume: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioVolume(_ outVolume: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioVolumeInfo(_ outMinVolume: UnsafeMutablePointer<UInt16>?, _ outCurVolume: UnsafeMutablePointer<UInt16>?, _ outMaxVolume: UnsafeMutablePointer<UInt16>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDHasMenu(_ inMenu: DVDMenu, _ outHasMenu: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsOnMenu(_ outOnMenu: UnsafeMutablePointer<DarwinBoolean>?, _ outMenu: UnsafeMutablePointer<DVDMenu>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGoToMenu(_ inMenu: DVDMenu) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDReturnToTitle() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGoBackOneLevel() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDDoUserNavigation(_ inNavigation: DVDUserNavigation) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func DVDDoButtonActivate(_ inIndex: Int32) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func DVDGetButtoninfo(_ numberOfButtons: UnsafeMutablePointer<UInt32>?, _ selectedButton: UnsafeMutablePointer<UInt32>?, _ forcedActivateButton: UnsafeMutablePointer<UInt32>?, _ userButtonOffset: UnsafeMutablePointer<UInt32>?, _ numberOfUserButtons: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func DVDGetButtonPosition(_ index: UInt32, _ outRect: UnsafeMutablePointer<CGRect>, _ autoAction: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDDoMenuCGClick(_ inPt: UnsafeMutablePointer<CGPoint>, _ outIndex: UnsafeMutablePointer<Int32>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func DVDDoMenuCGMouseOver(_ inPt: UnsafeMutablePointer<CGPoint>, _ outIndex: UnsafeMutablePointer<Int32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetMediaUniqueID(_ outDiscID: UnsafeMutablePointer<UInt8>!) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetMediaVolumeName(_ outDiscVolumeName: UnsafeMutablePointer<UnsafeMutablePointer<Int8>>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func DVDGetMediaVolumeCFName(_ outDiscVolumeCFName: UnsafeMutablePointer<Unmanaged<CFString>>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetTitle(_ inTitleNum: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetTitle(_ outTitleNum: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetNumTitles(_ outNumTitles: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDHasPreviousChapter(_ outHasChapter: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDHasNextChapter(_ outHasChapter: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetChapter(_ inChapterNum: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetChapter(_ outChapterNum: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetNumChapters(_ inTitleNum: UInt16, _ outNumChapters: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDPreviousChapter() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDNextChapter() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetAngle(_ inAngleNum: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAngle(_ outAngleNum: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetNumAngles(_ outNumAngles: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDDisplaySubPicture(_ inDisplay: Bool) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsDisplayingSubPicture(_ outDisplayingSubPicture: UnsafeMutablePointer<DarwinBoolean>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetSubPictureStream(_ inStreamNum: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetSubPictureStream(_ outStreamNum: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetNumSubPictureStreams(_ outNumStreams: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetAudioStream(_ inStreamNum: UInt16) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioStream(_ outStreamNum: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetNumAudioStreams(_ outNumStreams: UnsafeMutablePointer<UInt16>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetDefaultSubPictureLanguageCode(_ inCode: DVDLanguageCode, _ inExtension: DVDSubpictureExtensionCode) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetSubPictureLanguageCode(_ outCode: UnsafeMutablePointer<DVDLanguageCode>?, _ outExtension: UnsafeMutablePointer<DVDSubpictureExtensionCode>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetSubPictureLanguageCodeByStream(_ inStreamNum: UInt16, _ outCode: UnsafeMutablePointer<DVDLanguageCode>?, _ outExtension: UnsafeMutablePointer<DVDSubpictureExtensionCode>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetDefaultAudioLanguageCode(_ inCode: DVDLanguageCode, _ inExtension: DVDAudioExtensionCode) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioLanguageCode(_ outCode: UnsafeMutablePointer<DVDLanguageCode>?, _ outExtension: UnsafeMutablePointer<DVDAudioExtensionCode>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetAudioLanguageCodeByStream(_ inStreamNum: UInt16, _ outCode: UnsafeMutablePointer<DVDLanguageCode>?, _ outExtension: UnsafeMutablePointer<DVDAudioExtensionCode>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetDefaultMenuLanguageCode(_ inCode: DVDLanguageCode) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetMenuLanguageCode(_ outCode: UnsafeMutablePointer<DVDLanguageCode>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetBookmark(_ outBookMarkData: UnsafeMutablePointer<Void>?, _ ioBookMarkDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGotoBookmark(_ inBookMarkData: UnsafeMutablePointer<Void>, _ inBookMarkDataSize: UInt32) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetLastPlayBookmark(_ outBookMarkData: UnsafeMutablePointer<Void>?, _ ioBookMarkDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetLastPlayBookmark(_ inBookMarkData: UnsafeMutablePointer<Void>, _ inBookMarkDataSize: UInt32) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDClearLastPlayBookmark() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetDiscRegionCode(_ outCode: UnsafeMutablePointer<DVDRegionCode>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetDriveRegionCode(_ outCode: UnsafeMutablePointer<DVDRegionCode>?, _ outNumberChangesLeft: UnsafeMutablePointer<Int16>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetDriveRegionCode(_ inCode: DVDRegionCode, _ inAuthorization: AuthorizationRef) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDEnableWebAccess(_ inEnable: Bool) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func DVDGetGPRMValue(_ index: UInt32, _ value: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSetFatalErrorCallBack(_ inCallBackProc: DVDFatalErrCallBackFunctionPtr, _ inRefCon: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDRegisterEventCallBack(_ inCallBackProc: DVDEventCallBackFunctionPtr, _ inCode: UnsafeMutablePointer<DVDEventCode>, _ inCodeCount: UInt32, _ inRefCon: UnsafeMutablePointer<Void>?, _ outCallBackID: UnsafeMutablePointer<DVDEventCallBackRef>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDUnregisterEventCallBack(_ inCallBackID: DVDEventCallBackRef) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDIsRegisteredEventCallBack(_ inCallBackID: DVDEventCallBackRef) -> Bool
@available(OSX 10.3, *)
@discardableResult
func DVDSetTimeEventRate(_ inMilliseconds: UInt32) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDGetTimeEventRate(_ outMilliseconds: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDSleep() -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func DVDWakeUp() -> OSStatus
