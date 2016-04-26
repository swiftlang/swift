
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeTokenType: String
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeLexicalClass: String
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeNameType: String
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeNameTypeOrLexicalClass: String
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeLemma: String
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeLanguage: String
@available(watchOS 2.0, *)
let NSLinguisticTagSchemeScript: String
@available(watchOS 2.0, *)
let NSLinguisticTagWord: String
@available(watchOS 2.0, *)
let NSLinguisticTagPunctuation: String
@available(watchOS 2.0, *)
let NSLinguisticTagWhitespace: String
@available(watchOS 2.0, *)
let NSLinguisticTagOther: String
@available(watchOS 2.0, *)
let NSLinguisticTagNoun: String
@available(watchOS 2.0, *)
let NSLinguisticTagVerb: String
@available(watchOS 2.0, *)
let NSLinguisticTagAdjective: String
@available(watchOS 2.0, *)
let NSLinguisticTagAdverb: String
@available(watchOS 2.0, *)
let NSLinguisticTagPronoun: String
@available(watchOS 2.0, *)
let NSLinguisticTagDeterminer: String
@available(watchOS 2.0, *)
let NSLinguisticTagParticle: String
@available(watchOS 2.0, *)
let NSLinguisticTagPreposition: String
@available(watchOS 2.0, *)
let NSLinguisticTagNumber: String
@available(watchOS 2.0, *)
let NSLinguisticTagConjunction: String
@available(watchOS 2.0, *)
let NSLinguisticTagInterjection: String
@available(watchOS 2.0, *)
let NSLinguisticTagClassifier: String
@available(watchOS 2.0, *)
let NSLinguisticTagIdiom: String
@available(watchOS 2.0, *)
let NSLinguisticTagOtherWord: String
@available(watchOS 2.0, *)
let NSLinguisticTagSentenceTerminator: String
@available(watchOS 2.0, *)
let NSLinguisticTagOpenQuote: String
@available(watchOS 2.0, *)
let NSLinguisticTagCloseQuote: String
@available(watchOS 2.0, *)
let NSLinguisticTagOpenParenthesis: String
@available(watchOS 2.0, *)
let NSLinguisticTagCloseParenthesis: String
@available(watchOS 2.0, *)
let NSLinguisticTagWordJoiner: String
@available(watchOS 2.0, *)
let NSLinguisticTagDash: String
@available(watchOS 2.0, *)
let NSLinguisticTagOtherPunctuation: String
@available(watchOS 2.0, *)
let NSLinguisticTagParagraphBreak: String
@available(watchOS 2.0, *)
let NSLinguisticTagOtherWhitespace: String
@available(watchOS 2.0, *)
let NSLinguisticTagPersonalName: String
@available(watchOS 2.0, *)
let NSLinguisticTagPlaceName: String
@available(watchOS 2.0, *)
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
@available(watchOS 2.0, *)
class NSLinguisticTagger : NSObject {
  @available(watchOS 2.0, *)
  init(tagSchemes tagSchemes: [String], options opts: Int)
  @available(watchOS 2.0, *)
  var tagSchemes: [String] { get }
  @available(watchOS 2.0, *)
  var string: String?
  @available(watchOS 2.0, *)
  @discardableResult
  class func availableTagSchemes(forLanguage language: String) -> [String]
  @available(watchOS 2.0, *)
  func setOrthography(_ orthography: NSOrthography?, range range: NSRange)
  @available(watchOS 2.0, *)
  @discardableResult
  func orthography(at charIndex: Int, effectiveRange effectiveRange: NSRangePointer?) -> NSOrthography?
  @available(watchOS 2.0, *)
  func stringEdited(in newRange: NSRange, changeInLength delta: Int)
  @available(watchOS 2.0, *)
  func enumerateTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(watchOS 2.0, *)
  @discardableResult
  func sentenceRange(for range: NSRange) -> NSRange
  @available(watchOS 2.0, *)
  @discardableResult
  func tag(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?) -> String?
  @available(watchOS 2.0, *)
  @discardableResult
  func tags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(watchOS 2.0, *)
  @discardableResult
  func possibleTags(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?, scores scores: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]?
}
extension NSString {
  @available(watchOS 2.0, *)
  @discardableResult
  func linguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(watchOS 2.0, *)
  func enumerateLinguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
}
