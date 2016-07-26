
@available(iOS 5.0, *)
let NSLinguisticTagSchemeTokenType: String
@available(iOS 5.0, *)
let NSLinguisticTagSchemeLexicalClass: String
@available(iOS 5.0, *)
let NSLinguisticTagSchemeNameType: String
@available(iOS 5.0, *)
let NSLinguisticTagSchemeNameTypeOrLexicalClass: String
@available(iOS 5.0, *)
let NSLinguisticTagSchemeLemma: String
@available(iOS 5.0, *)
let NSLinguisticTagSchemeLanguage: String
@available(iOS 5.0, *)
let NSLinguisticTagSchemeScript: String
@available(iOS 5.0, *)
let NSLinguisticTagWord: String
@available(iOS 5.0, *)
let NSLinguisticTagPunctuation: String
@available(iOS 5.0, *)
let NSLinguisticTagWhitespace: String
@available(iOS 5.0, *)
let NSLinguisticTagOther: String
@available(iOS 5.0, *)
let NSLinguisticTagNoun: String
@available(iOS 5.0, *)
let NSLinguisticTagVerb: String
@available(iOS 5.0, *)
let NSLinguisticTagAdjective: String
@available(iOS 5.0, *)
let NSLinguisticTagAdverb: String
@available(iOS 5.0, *)
let NSLinguisticTagPronoun: String
@available(iOS 5.0, *)
let NSLinguisticTagDeterminer: String
@available(iOS 5.0, *)
let NSLinguisticTagParticle: String
@available(iOS 5.0, *)
let NSLinguisticTagPreposition: String
@available(iOS 5.0, *)
let NSLinguisticTagNumber: String
@available(iOS 5.0, *)
let NSLinguisticTagConjunction: String
@available(iOS 5.0, *)
let NSLinguisticTagInterjection: String
@available(iOS 5.0, *)
let NSLinguisticTagClassifier: String
@available(iOS 5.0, *)
let NSLinguisticTagIdiom: String
@available(iOS 5.0, *)
let NSLinguisticTagOtherWord: String
@available(iOS 5.0, *)
let NSLinguisticTagSentenceTerminator: String
@available(iOS 5.0, *)
let NSLinguisticTagOpenQuote: String
@available(iOS 5.0, *)
let NSLinguisticTagCloseQuote: String
@available(iOS 5.0, *)
let NSLinguisticTagOpenParenthesis: String
@available(iOS 5.0, *)
let NSLinguisticTagCloseParenthesis: String
@available(iOS 5.0, *)
let NSLinguisticTagWordJoiner: String
@available(iOS 5.0, *)
let NSLinguisticTagDash: String
@available(iOS 5.0, *)
let NSLinguisticTagOtherPunctuation: String
@available(iOS 5.0, *)
let NSLinguisticTagParagraphBreak: String
@available(iOS 5.0, *)
let NSLinguisticTagOtherWhitespace: String
@available(iOS 5.0, *)
let NSLinguisticTagPersonalName: String
@available(iOS 5.0, *)
let NSLinguisticTagPlaceName: String
@available(iOS 5.0, *)
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
@available(iOS 5.0, *)
class NSLinguisticTagger : NSObject {
  @available(iOS 5.0, *)
  init(tagSchemes tagSchemes: [String], options opts: Int)
  @available(iOS 5.0, *)
  var tagSchemes: [String] { get }
  @available(iOS 5.0, *)
  var string: String?
  @available(iOS 5.0, *)
  @discardableResult
  class func availableTagSchemes(forLanguage language: String) -> [String]
  @available(iOS 5.0, *)
  func setOrthography(_ orthography: NSOrthography?, range range: NSRange)
  @available(iOS 5.0, *)
  @discardableResult
  func orthography(at charIndex: Int, effectiveRange effectiveRange: NSRangePointer?) -> NSOrthography?
  @available(iOS 5.0, *)
  func stringEdited(in newRange: NSRange, changeInLength delta: Int)
  @available(iOS 5.0, *)
  func enumerateTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(iOS 5.0, *)
  @discardableResult
  func sentenceRange(for range: NSRange) -> NSRange
  @available(iOS 5.0, *)
  @discardableResult
  func tag(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?) -> String?
  @available(iOS 5.0, *)
  @discardableResult
  func tags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(iOS 5.0, *)
  @discardableResult
  func possibleTags(at charIndex: Int, scheme tagScheme: String, tokenRange tokenRange: NSRangePointer?, sentenceRange sentenceRange: NSRangePointer?, scores scores: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]?
}
extension NSString {
  @available(iOS 5.0, *)
  @discardableResult
  func linguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, tokenRanges tokenRanges: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> [String]
  @available(iOS 5.0, *)
  func enumerateLinguisticTags(in range: NSRange, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = [], orthography orthography: NSOrthography?, using block: (String, NSRange, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
}
