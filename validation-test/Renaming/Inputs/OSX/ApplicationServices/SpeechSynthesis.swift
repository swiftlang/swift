
var kTextToSpeechSynthType: OSType { get }
var kTextToSpeechVoiceType: OSType { get }
var kTextToSpeechVoiceFileType: OSType { get }
var kTextToSpeechVoiceBundleType: OSType { get }
var kNoEndingProsody: Int32 { get }
var kNoSpeechInterrupt: Int32 { get }
var kPreflightThenPause: Int32 { get }
var kImmediate: Int32 { get }
var kEndOfWord: Int32 { get }
var kEndOfSentence: Int32 { get }
var soStatus: OSType { get }
var soErrors: OSType { get }
var soInputMode: OSType { get }
var soCharacterMode: OSType { get }
var soNumberMode: OSType { get }
var soRate: OSType { get }
var soPitchBase: OSType { get }
var soPitchMod: OSType { get }
var soVolume: OSType { get }
var soSynthType: OSType { get }
var soRecentSync: OSType { get }
var soPhonemeSymbols: OSType { get }
var soCurrentVoice: OSType { get }
var soCommandDelimiter: OSType { get }
var soReset: OSType { get }
var soCurrentA5: OSType { get }
var soRefCon: OSType { get }
var soTextDoneCallBack: OSType { get }
var soSpeechDoneCallBack: OSType { get }
var soSyncCallBack: OSType { get }
var soErrorCallBack: OSType { get }
var soPhonemeCallBack: OSType { get }
var soWordCallBack: OSType { get }
var soSynthExtension: OSType { get }
var soSoundOutput: OSType { get }
var soOutputToFileWithCFURL: OSType { get }
var soOutputToExtAudioFile: OSType { get }
var soOutputToAudioDevice: OSType { get }
var soPhonemeOptions: OSType { get }
var modeText: OSType { get }
var modePhonemes: OSType { get }
var modeTune: OSType { get }
var modeNormal: OSType { get }
var modeLiteral: OSType { get }
var soVoiceDescription: OSType { get }
var soVoiceFile: OSType { get }
var kSpeechGenerateTune: Int32 { get }
var kSpeechRelativePitch: Int32 { get }
var kSpeechRelativeDuration: Int32 { get }
var kSpeechShowSyllables: Int32 { get }
var kAudioUnitSubType_SpeechSynthesis: UInt32 { get }
var kAudioUnitProperty_Voice: UInt32 { get }
var kAudioUnitProperty_SpeechChannel: UInt32 { get }
struct SpeechChannelRecord {
  var data: (Int)
  init()
  init(data data: (Int))
}
typealias SpeechChannel = UnsafeMutablePointer<SpeechChannelRecord>
struct VoiceSpec {
  var creator: OSType
  var id: OSType
  init()
  init(creator creator: OSType, id id: OSType)
}
typealias VoiceSpecPtr = UnsafeMutablePointer<VoiceSpec>
var kNeuter: Int16 { get }
var kMale: Int16 { get }
var kFemale: Int16 { get }
struct VoiceDescription {
  var length: Int32
  var voice: VoiceSpec
  var version: Int32
  var name: Str63
  var comment: Str255
  var gender: Int16
  var age: Int16
  var script: Int16
  var language: Int16
  var region: Int16
  var reserved: (Int32, Int32, Int32, Int32)
  init()
  init(length length: Int32, voice voice: VoiceSpec, version version: Int32, name name: Str63, comment comment: Str255, gender gender: Int16, age age: Int16, script script: Int16, language language: Int16, region region: Int16, reserved reserved: (Int32, Int32, Int32, Int32))
}
struct VoiceFileInfo {
  var fileSpec: FSSpec
  var resID: Int16
  init()
  init(fileSpec fileSpec: FSSpec, resID resID: Int16)
}
struct SpeechStatusInfo {
  var outputBusy: DarwinBoolean
  var outputPaused: DarwinBoolean
  var inputBytesLeft: Int
  var phonemeCode: Int16
  init()
  init(outputBusy outputBusy: DarwinBoolean, outputPaused outputPaused: DarwinBoolean, inputBytesLeft inputBytesLeft: Int, phonemeCode phonemeCode: Int16)
}
struct SpeechErrorInfo {
  var count: Int16
  var oldest: OSErr
  var oldPos: Int
  var newest: OSErr
  var newPos: Int
  init()
  init(count count: Int16, oldest oldest: OSErr, oldPos oldPos: Int, newest newest: OSErr, newPos newPos: Int)
}
struct SpeechVersionInfo {
  var synthType: OSType
  var synthSubType: OSType
  var synthManufacturer: OSType
  var synthFlags: Int32
  var synthVersion: NumVersion
  init()
  init(synthType synthType: OSType, synthSubType synthSubType: OSType, synthManufacturer synthManufacturer: OSType, synthFlags synthFlags: Int32, synthVersion synthVersion: NumVersion)
}
struct PhonemeInfo {
  var opcode: Int16
  var phStr: Str15
  var exampleStr: Str31
  var hiliteStart: Int16
  var hiliteEnd: Int16
  init()
  init(opcode opcode: Int16, phStr phStr: Str15, exampleStr exampleStr: Str31, hiliteStart hiliteStart: Int16, hiliteEnd hiliteEnd: Int16)
}
struct PhonemeDescriptor {
  var phonemeCount: Int16
  var thePhonemes: (PhonemeInfo)
  init()
  init(phonemeCount phonemeCount: Int16, thePhonemes thePhonemes: (PhonemeInfo))
}
struct SpeechXtndData {
  var synthCreator: OSType
  var synthData: (UInt8, UInt8)
  init()
  init(synthCreator synthCreator: OSType, synthData synthData: (UInt8, UInt8))
}
struct DelimiterInfo {
  var startDelimiter: (UInt8, UInt8)
  var endDelimiter: (UInt8, UInt8)
  init()
  init(startDelimiter startDelimiter: (UInt8, UInt8), endDelimiter endDelimiter: (UInt8, UInt8))
}
@available(OSX 10.5, *)
let kSpeechStatusProperty: CFString
@available(OSX 10.5, *)
let kSpeechErrorsProperty: CFString
@available(OSX 10.5, *)
let kSpeechInputModeProperty: CFString
@available(OSX 10.5, *)
let kSpeechCharacterModeProperty: CFString
@available(OSX 10.5, *)
let kSpeechNumberModeProperty: CFString
@available(OSX 10.5, *)
let kSpeechRateProperty: CFString
@available(OSX 10.5, *)
let kSpeechPitchBaseProperty: CFString
@available(OSX 10.5, *)
let kSpeechPitchModProperty: CFString
@available(OSX 10.5, *)
let kSpeechVolumeProperty: CFString
@available(OSX 10.5, *)
let kSpeechSynthesizerInfoProperty: CFString
@available(OSX 10.5, *)
let kSpeechRecentSyncProperty: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeSymbolsProperty: CFString
@available(OSX 10.5, *)
let kSpeechCurrentVoiceProperty: CFString
@available(OSX 10.5, *)
let kSpeechCommandDelimiterProperty: CFString
@available(OSX 10.5, *)
let kSpeechResetProperty: CFString
@available(OSX 10.5, *)
let kSpeechOutputToFileURLProperty: CFString
@available(OSX 10.6, *)
let kSpeechOutputToExtAudioFileProperty: CFString
@available(OSX 10.6, *)
let kSpeechOutputToAudioDeviceProperty: CFString
@available(OSX 10.9, *)
let kSpeechOutputToFileDescriptorProperty: CFString
@available(OSX 10.9, *)
let kSpeechAudioOutputFormatProperty: CFString
@available(OSX 10.9, *)
let kSpeechOutputChannelMapProperty: CFString
@available(OSX 10.5, *)
let kSpeechRefConProperty: CFString
@available(OSX 10.5, *)
let kSpeechTextDoneCallBack: CFString
@available(OSX 10.5, *)
let kSpeechSpeechDoneCallBack: CFString
@available(OSX 10.5, *)
let kSpeechSyncCallBack: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeCallBack: CFString
@available(OSX 10.5, *)
let kSpeechErrorCFCallBack: CFString
@available(OSX 10.5, *)
let kSpeechWordCFCallBack: CFString
@available(OSX 10.6, *)
let kSpeechPhonemeOptionsProperty: CFString
@available(OSX 10.6, *)
let kSpeechAudioUnitProperty: CFString
@available(OSX 10.6, *)
let kSpeechAudioGraphProperty: CFString
@available(OSX 10.9, *)
let kSpeechSynthExtensionProperty: CFString
@available(OSX 10.5, *)
let kSpeechModeText: CFString
@available(OSX 10.5, *)
let kSpeechModePhoneme: CFString
@available(OSX 10.6, *)
let kSpeechModeTune: CFString
@available(OSX 10.5, *)
let kSpeechModeNormal: CFString
@available(OSX 10.5, *)
let kSpeechModeLiteral: CFString
@available(OSX 10.5, *)
let kSpeechNoEndingProsody: CFString
@available(OSX 10.5, *)
let kSpeechNoSpeechInterrupt: CFString
@available(OSX 10.5, *)
let kSpeechPreflightThenPause: CFString
@available(OSX 10.5, *)
let kSpeechStatusOutputBusy: CFString
@available(OSX 10.5, *)
let kSpeechStatusOutputPaused: CFString
@available(OSX 10.5, *)
let kSpeechStatusNumberOfCharactersLeft: CFString
@available(OSX 10.5, *)
let kSpeechStatusPhonemeCode: CFString
@available(OSX 10.5, *)
let kSpeechErrorCount: CFString
@available(OSX 10.5, *)
let kSpeechErrorOldest: CFString
@available(OSX 10.5, *)
let kSpeechErrorOldestCharacterOffset: CFString
@available(OSX 10.5, *)
let kSpeechErrorNewest: CFString
@available(OSX 10.5, *)
let kSpeechErrorNewestCharacterOffset: CFString
@available(OSX 10.5, *)
let kSpeechSynthesizerInfoIdentifier: CFString
@available(OSX 10.5, *)
let kSpeechSynthesizerInfoManufacturer: CFString
@available(OSX 10.5, *)
let kSpeechSynthesizerInfoVersion: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeInfoOpcode: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeInfoSymbol: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeInfoExample: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeInfoHiliteStart: CFString
@available(OSX 10.5, *)
let kSpeechPhonemeInfoHiliteEnd: CFString
@available(OSX 10.5, *)
let kSpeechVoiceCreator: CFString
@available(OSX 10.5, *)
let kSpeechVoiceID: CFString
@available(OSX 10.5, *)
let kSpeechCommandPrefix: CFString
@available(OSX 10.5, *)
let kSpeechCommandSuffix: CFString
@available(OSX 10.5, *)
let kSpeechDictionaryLocaleIdentifier: CFString
@available(OSX 10.5, *)
let kSpeechDictionaryModificationDate: CFString
@available(OSX 10.5, *)
let kSpeechDictionaryPronunciations: CFString
@available(OSX 10.5, *)
let kSpeechDictionaryAbbreviations: CFString
@available(OSX 10.5, *)
let kSpeechDictionaryEntrySpelling: CFString
@available(OSX 10.5, *)
let kSpeechDictionaryEntryPhonemes: CFString
@available(OSX 10.5, *)
let kSpeechErrorCallbackSpokenString: CFString
@available(OSX 10.5, *)
let kSpeechErrorCallbackCharacterOffset: CFString
typealias SpeechTextDoneProcPtr = @convention(c) (SpeechChannel, SRefCon, UnsafeMutablePointer<UnsafePointer<Void>?>?, UnsafeMutablePointer<UInt>, UnsafeMutablePointer<Int32>) -> Void
typealias SpeechDoneProcPtr = @convention(c) (SpeechChannel, SRefCon) -> Void
typealias SpeechSyncProcPtr = @convention(c) (SpeechChannel, SRefCon, OSType) -> Void
typealias SpeechErrorProcPtr = @convention(c) (SpeechChannel, SRefCon, OSErr, Int) -> Void
typealias SpeechPhonemeProcPtr = @convention(c) (SpeechChannel, SRefCon, Int16) -> Void
typealias SpeechWordProcPtr = @convention(c) (SpeechChannel, SRefCon, UInt, UInt16) -> Void
typealias SpeechTextDoneUPP = SpeechTextDoneProcPtr
typealias SpeechDoneUPP = SpeechDoneProcPtr
typealias SpeechSyncUPP = SpeechSyncProcPtr
typealias SpeechErrorUPP = SpeechErrorProcPtr
typealias SpeechPhonemeUPP = SpeechPhonemeProcPtr
typealias SpeechWordUPP = SpeechWordProcPtr
typealias SpeechErrorCFProcPtr = @convention(c) (SpeechChannel, SRefCon, CFError) -> Void
typealias SpeechWordCFProcPtr = @convention(c) (SpeechChannel, SRefCon, CFString, CFRange) -> Void
@discardableResult
func SpeechManagerVersion() -> NumVersion
@discardableResult
func MakeVoiceSpec(_ creator: OSType, _ id: OSType, _ voice: UnsafeMutablePointer<VoiceSpec>) -> OSErr
@discardableResult
func CountVoices(_ numVoices: UnsafeMutablePointer<Int16>) -> OSErr
@discardableResult
func GetIndVoice(_ index: Int16, _ voice: UnsafeMutablePointer<VoiceSpec>) -> OSErr
@discardableResult
func GetVoiceDescription(_ voice: UnsafePointer<VoiceSpec>?, _ info: UnsafeMutablePointer<VoiceDescription>?, _ infoLength: Int) -> OSErr
@discardableResult
func GetVoiceInfo(_ voice: UnsafePointer<VoiceSpec>?, _ selector: OSType, _ voiceInfo: UnsafeMutablePointer<Void>) -> OSErr
@discardableResult
func NewSpeechChannel(_ voice: UnsafeMutablePointer<VoiceSpec>?, _ chan: UnsafeMutablePointer<SpeechChannel?>) -> OSErr
@discardableResult
func DisposeSpeechChannel(_ chan: SpeechChannel) -> OSErr
@discardableResult
func StopSpeech(_ chan: SpeechChannel) -> OSErr
@discardableResult
func StopSpeechAt(_ chan: SpeechChannel, _ whereToStop: Int32) -> OSErr
@discardableResult
func PauseSpeechAt(_ chan: SpeechChannel, _ whereToPause: Int32) -> OSErr
@discardableResult
func ContinueSpeech(_ chan: SpeechChannel) -> OSErr
@discardableResult
func SpeechBusy() -> Int16
@discardableResult
func SpeechBusySystemWide() -> Int16
@discardableResult
func SetSpeechRate(_ chan: SpeechChannel, _ rate: Fixed) -> OSErr
@discardableResult
func GetSpeechRate(_ chan: SpeechChannel, _ rate: UnsafeMutablePointer<Fixed>) -> OSErr
@discardableResult
func SetSpeechPitch(_ chan: SpeechChannel, _ pitch: Fixed) -> OSErr
@discardableResult
func GetSpeechPitch(_ chan: SpeechChannel, _ pitch: UnsafeMutablePointer<Fixed>) -> OSErr
@available(OSX 10.5, *)
@discardableResult
func SpeakCFString(_ chan: SpeechChannel, _ aString: CFString, _ options: CFDictionary?) -> OSErr
@available(OSX 10.5, *)
@discardableResult
func UseSpeechDictionary(_ chan: SpeechChannel, _ speechDictionary: CFDictionary) -> OSErr
@available(OSX 10.5, *)
@discardableResult
func CopyPhonemesFromText(_ chan: SpeechChannel, _ text: CFString, _ phonemes: UnsafeMutablePointer<CFString?>) -> OSErr
@available(OSX 10.5, *)
@discardableResult
func CopySpeechProperty(_ chan: SpeechChannel, _ property: CFString, _ object: UnsafeMutablePointer<CFTypeRef?>) -> OSErr
@available(OSX 10.5, *)
@discardableResult
func SetSpeechProperty(_ chan: SpeechChannel, _ property: CFString, _ object: CFTypeRef?) -> OSErr
@available(OSX 10.6, *)
@discardableResult
func SpeechSynthesisRegisterModuleURL(_ url: CFURL) -> OSErr
@available(OSX 10.6, *)
@discardableResult
func SpeechSynthesisUnregisterModuleURL(_ url: CFURL) -> OSErr
