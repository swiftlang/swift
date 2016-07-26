
class NSSpellServer : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged NSSpellServerDelegate?
  @discardableResult
  func registerLanguage(_ language: String?, byVendor vendor: String?) -> Bool
  @discardableResult
  func isWord(inUserDictionaries word: String, caseSensitive flag: Bool) -> Bool
  func run()
}
struct __ssFlags {
  var delegateLearnsWords: UInt32
  var delegateForgetsWords: UInt32
  var busy: UInt32
  var _reserved: UInt32
  init()
  init(delegateLearnsWords delegateLearnsWords: UInt32, delegateForgetsWords delegateForgetsWords: UInt32, busy busy: UInt32, _reserved _reserved: UInt32)
}
protocol NSSpellServerDelegate : NSObjectProtocol {
  @discardableResult
  optional func spellServer(_ sender: NSSpellServer, findMisspelledWordIn stringToCheck: String, language language: String, wordCount wordCount: UnsafeMutablePointer<Int>, countOnly countOnly: Bool) -> NSRange
  @discardableResult
  optional func spellServer(_ sender: NSSpellServer, suggestGuessesForWord word: String, inLanguage language: String) -> [String]?
  optional func spellServer(_ sender: NSSpellServer, didLearnWord word: String, inLanguage language: String)
  optional func spellServer(_ sender: NSSpellServer, didForgetWord word: String, inLanguage language: String)
  @discardableResult
  optional func spellServer(_ sender: NSSpellServer, suggestCompletionsForPartialWordRange range: NSRange, in string: String, language language: String) -> [String]?
  @available(OSX 10.5, *)
  @discardableResult
  optional func spellServer(_ sender: NSSpellServer, checkGrammarIn stringToCheck: String, language language: String?, details details: AutoreleasingUnsafeMutablePointer<NSArray?>?) -> NSRange
  @available(OSX 10.6, *)
  @discardableResult
  optional func spellServer(_ sender: NSSpellServer, check stringToCheck: String, offset offset: Int, types checkingTypes: NSTextCheckingTypes, options options: [String : AnyObject]? = [:], orthography orthography: NSOrthography?, wordCount wordCount: UnsafeMutablePointer<Int>) -> [NSTextCheckingResult]?
  @available(OSX 10.7, *)
  optional func spellServer(_ sender: NSSpellServer, recordResponse response: Int, toCorrection correction: String, forWord word: String, language language: String)
}
@available(OSX 10.5, *)
let NSGrammarRange: String
@available(OSX 10.5, *)
let NSGrammarUserDescription: String
@available(OSX 10.5, *)
let NSGrammarCorrections: String
