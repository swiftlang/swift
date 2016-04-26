
var DRCDTextEncodingISOLatin1Modified: Int { get }
var DRCDTextEncodingASCII: Int { get }
var DRCDTextGenreCodeUnknown: Int { get }
var DRCDTextGenreCodeAdultContemporary: Int { get }
var DRCDTextGenreCodeAlternativeRock: Int { get }
var DRCDTextGenreCodeChildrens: Int { get }
var DRCDTextGenreCodeClassical: Int { get }
var DRCDTextGenreCodeContemporaryChristian: Int { get }
var DRCDTextGenreCodeCountry: Int { get }
var DRCDTextGenreCodeDance: Int { get }
var DRCDTextGenreCodeEasyListening: Int { get }
var DRCDTextGenreCodeErotic: Int { get }
var DRCDTextGenreCodeFolk: Int { get }
var DRCDTextGenreCodeGospel: Int { get }
var DRCDTextGenreCodeHipHop: Int { get }
var DRCDTextGenreCodeJazz: Int { get }
var DRCDTextGenreCodeLatin: Int { get }
var DRCDTextGenreCodeMusical: Int { get }
var DRCDTextGenreCodeNewAge: Int { get }
var DRCDTextGenreCodeOpera: Int { get }
var DRCDTextGenreCodeOperetta: Int { get }
var DRCDTextGenreCodePop: Int { get }
var DRCDTextGenreCodeRap: Int { get }
var DRCDTextGenreCodeReggae: Int { get }
var DRCDTextGenreCodeRock: Int { get }
var DRCDTextGenreCodeRhythmAndBlues: Int { get }
var DRCDTextGenreCodeSoundEffects: Int { get }
var DRCDTextGenreCodeSoundtrack: Int { get }
var DRCDTextGenreCodeSpokenWord: Int { get }
var DRCDTextGenreCodeWorldMusic: Int { get }
class DRCDTextBlock : NSObject {
  @discardableResult
  class func arrayOfCDTextBlocks(fromPacks packs: NSData!) -> [AnyObject]!
  init!(language lang: String!, encoding enc: UInt)
  @discardableResult
  func properties() -> [NSObject : AnyObject]!
  func setProperties(_ properties: [NSObject : AnyObject]!)
  @discardableResult
  func trackDictionaries() -> [AnyObject]!
  func setTrackDictionaries(_ tracks: [AnyObject]!)
  @discardableResult
  func object(forKey key: String!, ofTrack trackIndex: Int) -> AnyObject!
  func setObject(_ value: AnyObject!, forKey key: String!, ofTrack trackIndex: Int)
  @discardableResult
  func flatten() -> Int
}
extension DRCDTextBlock {
  @discardableResult
  func language() -> String!
  @discardableResult
  func encoding() -> UInt
}
@available(OSX 10.4, *)
let DRCDTextLanguageKey: String
@available(OSX 10.4, *)
let DRCDTextCharacterCodeKey: String
@available(OSX 10.4, *)
let DRCDTextNSStringEncodingKey: String
@available(OSX 10.4, *)
let DRCDTextCopyrightAssertedForSpecialMessagesKey: String
@available(OSX 10.4, *)
let DRCDTextCopyrightAssertedForNamesKey: String
@available(OSX 10.4, *)
let DRCDTextCopyrightAssertedForTitlesKey: String
@available(OSX 10.4, *)
let DRCDTextTitleKey: String
@available(OSX 10.4, *)
let DRCDTextPerformerKey: String
@available(OSX 10.4, *)
let DRCDTextSongwriterKey: String
@available(OSX 10.4, *)
let DRCDTextComposerKey: String
@available(OSX 10.4, *)
let DRCDTextArrangerKey: String
@available(OSX 10.4, *)
let DRCDTextSpecialMessageKey: String
@available(OSX 10.4, *)
let DRCDTextDiscIdentKey: String
@available(OSX 10.4, *)
let DRCDTextGenreKey: String
@available(OSX 10.4, *)
let DRCDTextGenreCodeKey: String
@available(OSX 10.4, *)
let DRCDTextClosedKey: String
@available(OSX 10.4, *)
let DRCDTextMCNISRCKey: String
@available(OSX 10.4, *)
let DRCDTextTOCKey: String
@available(OSX 10.4, *)
let DRCDTextTOC2Key: String
@available(OSX 10.4, *)
let DRCDTextSizeKey: String
