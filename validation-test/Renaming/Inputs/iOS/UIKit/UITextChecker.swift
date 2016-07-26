
@available(iOS 3.2, *)
class UITextChecker : NSObject {
  @discardableResult
  func rangeOfMisspelledWord(in stringToCheck: String, range range: NSRange, startingAt startingOffset: Int, wrap wrapFlag: Bool, language language: String) -> NSRange
  @discardableResult
  func guesses(forWordRange range: NSRange, in string: String, language language: String) -> [AnyObject]?
  @discardableResult
  func completions(forPartialWordRange range: NSRange, in string: String?, language language: String) -> [AnyObject]?
  func ignoreWord(_ wordToIgnore: String)
  @discardableResult
  func ignoredWords() -> [AnyObject]?
  func setIgnoredWords(_ words: [AnyObject]?)
  class func learnWord(_ word: String)
  @discardableResult
  class func hasLearnedWord(_ word: String) -> Bool
  class func unlearnWord(_ word: String)
  @discardableResult
  class func availableLanguages() -> [AnyObject]
}
