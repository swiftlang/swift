
class NSSpellChecker : NSObject {
  @discardableResult
  class func shared() -> NSSpellChecker
  @discardableResult
  class func sharedSpellCheckerExists() -> Bool
  @discardableResult
  class func uniqueSpellDocumentTag() -> Int
  @discardableResult
  func checkSpelling(of stringToCheck: String, startingAt startingOffset: Int, language language: String?, wrap wrapFlag: Bool, inSpellDocumentWithTag tag: Int, wordCount wordCount: UnsafeMutablePointer<Int>?) -> NSRange
  @discardableResult
  func checkSpelling(of stringToCheck: String, startingAt startingOffset: Int) -> NSRange
  @discardableResult
  func countWords(in stringToCount: String, language language: String?) -> Int
  @available(OSX 10.5, *)
  @discardableResult
  func checkGrammar(of stringToCheck: String, startingAt startingOffset: Int, language language: String?, wrap wrapFlag: Bool, inSpellDocumentWithTag tag: Int, details details: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> NSRange
  @available(OSX 10.6, *)
  @discardableResult
  func check(_ stringToCheck: String, range range: NSRange, types checkingTypes: NSTextCheckingTypes, options options: [String : AnyObject]? = [:], inSpellDocumentWithTag tag: Int, orthography orthography: AutoreleasingUnsafeMutablePointer<NSOrthography?>?, wordCount wordCount: UnsafeMutablePointer<Int>?) -> [NSTextCheckingResult]
  @available(OSX 10.6, *)
  @discardableResult
  func requestChecking(of stringToCheck: String, range range: NSRange, types checkingTypes: NSTextCheckingTypes, options options: [String : AnyObject]? = [:], inSpellDocumentWithTag tag: Int, completionHandler completionHandler: ((Int, [NSTextCheckingResult], NSOrthography, Int) -> Void)? = nil) -> Int
  @available(OSX 10.6, *)
  @discardableResult
  func menu(for result: NSTextCheckingResult, string checkedString: String, options options: [String : AnyObject]? = [:], atLocation location: NSPoint, in view: NSView) -> NSMenu?
  @available(OSX 10.6, *)
  @discardableResult
  func userQuotesArray(forLanguage language: String) -> [String]
  @available(OSX 10.6, *)
  var userReplacementsDictionary: [String : String] { get }
  func updateSpellingPanel(withMisspelledWord word: String)
  @available(OSX 10.5, *)
  func updateSpellingPanel(withGrammarString string: String, detail detail: [String : AnyObject])
  var spellingPanel: NSPanel { get }
  var accessoryView: NSView?
  @available(OSX 10.6, *)
  var substitutionsPanel: NSPanel { get }
  @available(OSX 10.6, *)
  var substitutionsPanelAccessoryViewController: NSViewController?
  @available(OSX 10.6, *)
  func updatePanels()
  func ignoreWord(_ wordToIgnore: String, inSpellDocumentWithTag tag: Int)
  @discardableResult
  func ignoredWordsInSpellDocument(withTag tag: Int) -> [String]?
  func setIgnoredWords(_ words: [String], inSpellDocumentWithTag tag: Int)
  @available(OSX 10.6, *)
  @discardableResult
  func guesses(forWordRange range: NSRange, in string: String, language language: String?, inSpellDocumentWithTag tag: Int) -> [String]?
  @available(OSX 10.7, *)
  @discardableResult
  func correction(forWordRange range: NSRange, in string: String, language language: String, inSpellDocumentWithTag tag: Int) -> String?
  @discardableResult
  func completions(forPartialWordRange range: NSRange, in string: String, language language: String?, inSpellDocumentWithTag tag: Int) -> [String]?
  @available(OSX 10.7, *)
  @discardableResult
  func language(forWordRange range: NSRange, in string: String, orthography orthography: NSOrthography?) -> String?
  func closeSpellDocument(withTag tag: Int)
  @available(OSX 10.7, *)
  func record(_ response: NSCorrectionResponse, toCorrection correction: String, forWord word: String, language language: String?, inSpellDocumentWithTag tag: Int)
  @available(OSX 10.7, *)
  func showCorrectionIndicator(of type: NSCorrectionIndicatorType, primaryString primaryString: String, alternativeStrings alternativeStrings: [String], forStringIn rectOfTypedString: NSRect, view view: NSView, completionHandler completionBlock: ((String!) -> Void)? = nil)
  @available(OSX 10.7, *)
  func dismissCorrectionIndicator(for view: NSView)
  @available(OSX 10.5, *)
  var availableLanguages: [String] { get }
  @available(OSX 10.6, *)
  var userPreferredLanguages: [String] { get }
  @available(OSX 10.6, *)
  var automaticallyIdentifiesLanguages: Bool
  func setWordFieldStringValue(_ aString: String)
  func learnWord(_ word: String)
  @available(OSX 10.5, *)
  @discardableResult
  func hasLearnedWord(_ word: String) -> Bool
  @available(OSX 10.5, *)
  func unlearnWord(_ word: String)
  @available(OSX 10.7, *)
  @discardableResult
  class func isAutomaticTextReplacementEnabled() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  class func isAutomaticSpellingCorrectionEnabled() -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  class func isAutomaticQuoteSubstitutionEnabled() -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  class func isAutomaticDashSubstitutionEnabled() -> Bool
  @discardableResult
  func language() -> String
  @discardableResult
  func setLanguage(_ language: String) -> Bool
}
struct __scFlags {
  var autoShowGuesses: UInt32
  var needDelayedGuess: UInt32
  var unignoreInProgress: UInt32
  var wordFieldEdited: UInt32
  var inSpelling: UInt32
  var reconnectSpelling: UInt32
  var inGrammar: UInt32
  var reconnectGrammar: UInt32
  var languageIdentification: UInt32
  var languagesHidden: UInt32
  var quotesByLanguage: UInt32
  var _reserved: UInt32
  init()
  init(autoShowGuesses autoShowGuesses: UInt32, needDelayedGuess needDelayedGuess: UInt32, unignoreInProgress unignoreInProgress: UInt32, wordFieldEdited wordFieldEdited: UInt32, inSpelling inSpelling: UInt32, reconnectSpelling reconnectSpelling: UInt32, inGrammar inGrammar: UInt32, reconnectGrammar reconnectGrammar: UInt32, languageIdentification languageIdentification: UInt32, languagesHidden languagesHidden: UInt32, quotesByLanguage quotesByLanguage: UInt32, _reserved _reserved: UInt32)
}
@available(OSX 10.6, *)
let NSTextCheckingOrthographyKey: String
@available(OSX 10.6, *)
let NSTextCheckingQuotesKey: String
@available(OSX 10.6, *)
let NSTextCheckingReplacementsKey: String
@available(OSX 10.6, *)
let NSTextCheckingReferenceDateKey: String
@available(OSX 10.6, *)
let NSTextCheckingReferenceTimeZoneKey: String
@available(OSX 10.6, *)
let NSTextCheckingDocumentURLKey: String
@available(OSX 10.6, *)
let NSTextCheckingDocumentTitleKey: String
@available(OSX 10.6, *)
let NSTextCheckingDocumentAuthorKey: String
@available(OSX 10.7, *)
let NSTextCheckingRegularExpressionsKey: String
enum NSCorrectionResponse : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case accepted
  case rejected
  case ignored
  case edited
  case reverted
}
enum NSCorrectionIndicatorType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case reversion
  case guesses
}
@available(OSX 10.7, *)
let NSSpellCheckerDidChangeAutomaticSpellingCorrectionNotification: String
@available(OSX 10.7, *)
let NSSpellCheckerDidChangeAutomaticTextReplacementNotification: String
@available(OSX 10.9, *)
let NSSpellCheckerDidChangeAutomaticQuoteSubstitutionNotification: String
@available(OSX 10.9, *)
let NSSpellCheckerDidChangeAutomaticDashSubstitutionNotification: String
extension NSSpellChecker {
}
