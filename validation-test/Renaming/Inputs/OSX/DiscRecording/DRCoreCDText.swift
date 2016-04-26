
var kDRCDTextEncodingISOLatin1Modified: Int { get }
var kDRCDTextEncodingASCII: Int { get }
var kDRCDTextGenreCodeUnknown: Int { get }
var kDRCDTextGenreCodeAdultContemporary: Int { get }
var kDRCDTextGenreCodeAlternativeRock: Int { get }
var kDRCDTextGenreCodeChildrens: Int { get }
var kDRCDTextGenreCodeClassical: Int { get }
var kDRCDTextGenreCodeContemporaryChristian: Int { get }
var kDRCDTextGenreCodeCountry: Int { get }
var kDRCDTextGenreCodeDance: Int { get }
var kDRCDTextGenreCodeEasyListening: Int { get }
var kDRCDTextGenreCodeErotic: Int { get }
var kDRCDTextGenreCodeFolk: Int { get }
var kDRCDTextGenreCodeGospel: Int { get }
var kDRCDTextGenreCodeHipHop: Int { get }
var kDRCDTextGenreCodeJazz: Int { get }
var kDRCDTextGenreCodeLatin: Int { get }
var kDRCDTextGenreCodeMusical: Int { get }
var kDRCDTextGenreCodeNewAge: Int { get }
var kDRCDTextGenreCodeOpera: Int { get }
var kDRCDTextGenreCodeOperetta: Int { get }
var kDRCDTextGenreCodePop: Int { get }
var kDRCDTextGenreCodeRap: Int { get }
var kDRCDTextGenreCodeReggae: Int { get }
var kDRCDTextGenreCodeRock: Int { get }
var kDRCDTextGenreCodeRhythmAndBlues: Int { get }
var kDRCDTextGenreCodeSoundEffects: Int { get }
var kDRCDTextGenreCodeSoundtrack: Int { get }
var kDRCDTextGenreCodeSpokenWord: Int { get }
var kDRCDTextGenreCodeWorldMusic: Int { get }
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockCreateArrayFromPackList(_ packs: CFData!) -> Unmanaged<CFArray>!
class DRCDTextBlockRef {
}
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockGetTypeID() -> CFTypeID
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockCreate(_ language: CFString!, _ encoding: CFStringEncoding) -> Unmanaged<DRCDTextBlockRef>!
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockGetProperties(_ block: DRCDTextBlockRef!) -> Unmanaged<CFDictionary>!
@available(OSX 10.4, *)
func DRCDTextBlockSetProperties(_ block: DRCDTextBlockRef!, _ properties: CFDictionary!)
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockGetTrackDictionaries(_ block: DRCDTextBlockRef!) -> Unmanaged<CFArray>!
@available(OSX 10.4, *)
func DRCDTextBlockSetTrackDictionaries(_ block: DRCDTextBlockRef!, _ array: CFArray!)
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockGetValue(_ block: DRCDTextBlockRef!, _ trackIndex: CFIndex, _ key: CFString!) -> Unmanaged<CFTypeRef>!
@available(OSX 10.4, *)
func DRCDTextBlockSetValue(_ block: DRCDTextBlockRef!, _ trackIndex: CFIndex, _ key: CFString!, _ value: CFTypeRef!)
@available(OSX 10.4, *)
@discardableResult
func DRCDTextBlockFlatten(_ block: DRCDTextBlockRef!) -> UInt32
@available(OSX 10.4, *)
let kDRCDTextLanguageKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextCharacterCodeKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextCFStringEncodingKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextCopyrightAssertedForSpecialMessagesKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextCopyrightAssertedForNamesKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextCopyrightAssertedForTitlesKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextTitleKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextPerformerKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextSongwriterKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextComposerKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextArrangerKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextSpecialMessageKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextDiscIdentKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextGenreKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextGenreCodeKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextClosedKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextMCNISRCKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextTOCKey: CFString!
@available(OSX 10.4, *)
let kDRCDTextTOC2Key: CFString!
@available(OSX 10.4, *)
let kDRCDTextSizeKey: CFString!
