
@available(OSX 10.7, *)
let NSLinguisticTagSchemeTokenType: String
@available(OSX 10.7, *)
let NSLinguisticTagSchemeLexicalClass: String
@available(OSX 10.7, *)
let NSLinguisticTagSchemeNameType: String
@available(OSX 10.7, *)
let NSLinguisticTagSchemeNameTypeOrLexicalClass: String
@available(OSX 10.7, *)
let NSLinguisticTagSchemeLemma: String
@available(OSX 10.7, *)
let NSLinguisticTagSchemeLanguage: String
@available(OSX 10.7, *)
let NSLinguisticTagSchemeScript: String
@available(OSX 10.7, *)
let NSLinguisticTagWord: String
@available(OSX 10.7, *)
let NSLinguisticTagPunctuation: String
@available(OSX 10.7, *)
let NSLinguisticTagWhitespace: String
@available(OSX 10.7, *)
let NSLinguisticTagOther: String
@available(OSX 10.7, *)
let NSLinguisticTagNoun: String
@available(OSX 10.7, *)
let NSLinguisticTagVerb: String
@available(OSX 10.7, *)
let NSLinguisticTagAdjective: String
@available(OSX 10.7, *)
let NSLinguisticTagAdverb: String
@available(OSX 10.7, *)
let NSLinguisticTagPronoun: String
@available(OSX 10.7, *)
let NSLinguisticTagDeterminer: String
@available(OSX 10.7, *)
let NSLinguisticTagParticle: String
@available(OSX 10.7, *)
let NSLinguisticTagPreposition: String
@available(OSX 10.7, *)
let NSLinguisticTagNumber: String
@available(OSX 10.7, *)
let NSLinguisticTagConjunction: String
@available(OSX 10.7, *)
let NSLinguisticTagInterjection: String
@available(OSX 10.7, *)
let NSLinguisticTagClassifier: String
@available(OSX 10.7, *)
let NSLinguisticTagIdiom: String
@available(OSX 10.7, *)
let NSLinguisticTagOtherWord: String
@available(OSX 10.7, *)
let NSLinguisticTagSentenceTerminator: String
@available(OSX 10.7, *)
let NSLinguisticTagOpenQuote: String
@available(OSX 10.7, *)
let NSLinguisticTagCloseQuote: String
@available(OSX 10.7, *)
let NSLinguisticTagOpenParenthesis: String
@available(OSX 10.7, *)
let NSLinguisticTagCloseParenthesis: String
@available(OSX 10.7, *)
let NSLinguisticTagWordJoiner: String
@available(OSX 10.7, *)
let NSLinguisticTagDash: String
@available(OSX 10.7, *)
let NSLinguisticTagOtherPunctuation: String
@available(OSX 10.7, *)
let NSLinguisticTagParagraphBreak: String
@available(OSX 10.7, *)
let NSLinguisticTagOtherWhitespace: String
@available(OSX 10.7, *)
let NSLinguisticTagPersonalName: String
@available(OSX 10.7, *)
let NSLinguisticTagPlaceName: String
@available(OSX 10.7, *)
let NSLinguisticTagOrganizationName: String
struct NSLinguisticTaggerOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var omitWords: NSLinguisticTaggerOptions { get }
  static var omitPunctuation: NSLinguisticTaggerOptions { get }
  static var omitWhitespace: NSLinguisticTaggerOptions { get }
  static var omitOther: NSLinguisticTaggerOptions { get }
  static var joinNames: NSLinguisticTaggerOptions { get }
}
@available(OSX 10.7, *)
class NSLinguisticTagger : NSObject {
  @available(OSX 10.7, *)
  init(tagSchemes tagSchemes: [String], options opts: Int)
  @available(OSX 10.7, *)
  var tagSchemes: [String] { get }
  @available(OSX 10.7, *)
  var string: String?
  @available(OSX 10.7, *)
  @discardableResult
  class func availableTagSchemes(forLanguage language: String) -> [String]
  @available(OSX 10.7, *)
  func setOrthography(_ orthography: NSOrthography?, range range: NSRange)
  @available(OSX 10.7, *)
  @discardableResult
  func orthography(at charIndex: Int, effectiveRange effectiveRange: NSRangePointer?) -> NSOrthography?
  @available(OSX 10.7, *)
  func stringEdited(in newRange: NSRange, changeInLength delta: Int)
  @available(OSX 10.7, *)
  func enumerateTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.7, *)
  @discardableResult
  func sentenceRange(for range: NSRange) -> NSRange
  @available(OSX 10.7, *)
  @discardableResult
  func tag(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?) -> String?
  @available(OSX 10.7, *)
  @discardableResult
  func tags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(OSX 10.7, *)
  @discardableResult
  func possibleTags(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?, scores scores: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]?
}
extension NSString {
  @available(OSX 10.7, *)
  @discardableResult
  func linguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(OSX 10.7, *)
  func enumerateLinguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
}
