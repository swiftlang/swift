
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeTokenType: String
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeLexicalClass: String
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeNameType: String
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeNameTypeOrLexicalClass: String
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeLemma: String
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeLanguage: String
@available(tvOS 5.0, *)
let NSLinguisticTagSchemeScript: String
@available(tvOS 5.0, *)
let NSLinguisticTagWord: String
@available(tvOS 5.0, *)
let NSLinguisticTagPunctuation: String
@available(tvOS 5.0, *)
let NSLinguisticTagWhitespace: String
@available(tvOS 5.0, *)
let NSLinguisticTagOther: String
@available(tvOS 5.0, *)
let NSLinguisticTagNoun: String
@available(tvOS 5.0, *)
let NSLinguisticTagVerb: String
@available(tvOS 5.0, *)
let NSLinguisticTagAdjective: String
@available(tvOS 5.0, *)
let NSLinguisticTagAdverb: String
@available(tvOS 5.0, *)
let NSLinguisticTagPronoun: String
@available(tvOS 5.0, *)
let NSLinguisticTagDeterminer: String
@available(tvOS 5.0, *)
let NSLinguisticTagParticle: String
@available(tvOS 5.0, *)
let NSLinguisticTagPreposition: String
@available(tvOS 5.0, *)
let NSLinguisticTagNumber: String
@available(tvOS 5.0, *)
let NSLinguisticTagConjunction: String
@available(tvOS 5.0, *)
let NSLinguisticTagInterjection: String
@available(tvOS 5.0, *)
let NSLinguisticTagClassifier: String
@available(tvOS 5.0, *)
let NSLinguisticTagIdiom: String
@available(tvOS 5.0, *)
let NSLinguisticTagOtherWord: String
@available(tvOS 5.0, *)
let NSLinguisticTagSentenceTerminator: String
@available(tvOS 5.0, *)
let NSLinguisticTagOpenQuote: String
@available(tvOS 5.0, *)
let NSLinguisticTagCloseQuote: String
@available(tvOS 5.0, *)
let NSLinguisticTagOpenParenthesis: String
@available(tvOS 5.0, *)
let NSLinguisticTagCloseParenthesis: String
@available(tvOS 5.0, *)
let NSLinguisticTagWordJoiner: String
@available(tvOS 5.0, *)
let NSLinguisticTagDash: String
@available(tvOS 5.0, *)
let NSLinguisticTagOtherPunctuation: String
@available(tvOS 5.0, *)
let NSLinguisticTagParagraphBreak: String
@available(tvOS 5.0, *)
let NSLinguisticTagOtherWhitespace: String
@available(tvOS 5.0, *)
let NSLinguisticTagPersonalName: String
@available(tvOS 5.0, *)
let NSLinguisticTagPlaceName: String
@available(tvOS 5.0, *)
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
@available(tvOS 5.0, *)
class NSLinguisticTagger : NSObject {
  @available(tvOS 5.0, *)
  init(tagSchemes tagSchemes: [String], options opts: Int)
  @available(tvOS 5.0, *)
  var tagSchemes: [String] { get }
  @available(tvOS 5.0, *)
  var string: String?
  @available(tvOS 5.0, *)
  @discardableResult
  class func availableTagSchemes(forLanguage language: String) -> [String]
  @available(tvOS 5.0, *)
  func setOrthography(_ orthography: NSOrthography?, range range: NSRange)
  @available(tvOS 5.0, *)
  @discardableResult
  func orthography(at charIndex: Int, effectiveRange effectiveRange: NSRangePointer?) -> NSOrthography?
  @available(tvOS 5.0, *)
  func stringEdited(in newRange: NSRange, changeInLength delta: Int)
  @available(tvOS 5.0, *)
  func enumerateTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 5.0, *)
  @discardableResult
  func sentenceRange(for range: NSRange) -> NSRange
  @available(tvOS 5.0, *)
  @discardableResult
  func tag(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?) -> String?
  @available(tvOS 5.0, *)
  @discardableResult
  func tags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(tvOS 5.0, *)
  @discardableResult
  func possibleTags(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?, scores scores: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]?
}
extension NSString {
  @available(tvOS 5.0, *)
  @discardableResult
  func linguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(tvOS 5.0, *)
  func enumerateLinguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
}
