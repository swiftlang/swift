
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerCopyBestStringLanguage(_ string: CFString!, _ range: CFRange) -> CFString!
class CFStringTokenizer {
}
var kCFStringTokenizerUnitWord: CFOptionFlags { get }
var kCFStringTokenizerUnitSentence: CFOptionFlags { get }
var kCFStringTokenizerUnitParagraph: CFOptionFlags { get }
var kCFStringTokenizerUnitLineBreak: CFOptionFlags { get }
var kCFStringTokenizerUnitWordBoundary: CFOptionFlags { get }
var kCFStringTokenizerAttributeLatinTranscription: CFOptionFlags { get }
var kCFStringTokenizerAttributeLanguage: CFOptionFlags { get }
struct CFStringTokenizerTokenType : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var normal: CFStringTokenizerTokenType { get }
  static var hasSubTokensMask: CFStringTokenizerTokenType { get }
  static var hasDerivedSubTokensMask: CFStringTokenizerTokenType { get }
  static var hasHasNumbersMask: CFStringTokenizerTokenType { get }
  static var hasNonLettersMask: CFStringTokenizerTokenType { get }
  static var iscjWordMask: CFStringTokenizerTokenType { get }
}
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerCreate(_ alloc: CFAllocator!, _ string: CFString!, _ range: CFRange, _ options: CFOptionFlags, _ locale: CFLocale!) -> CFStringTokenizer!
@available(OSX 10.5, *)
func CFStringTokenizerSetString(_ tokenizer: CFStringTokenizer!, _ string: CFString!, _ range: CFRange)
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerGoToTokenAtIndex(_ tokenizer: CFStringTokenizer!, _ index: CFIndex) -> CFStringTokenizerTokenType
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerAdvanceToNextToken(_ tokenizer: CFStringTokenizer!) -> CFStringTokenizerTokenType
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerGetCurrentTokenRange(_ tokenizer: CFStringTokenizer!) -> CFRange
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerCopyCurrentTokenAttribute(_ tokenizer: CFStringTokenizer!, _ attribute: CFOptionFlags) -> CFTypeRef!
@available(OSX 10.5, *)
@discardableResult
func CFStringTokenizerGetCurrentSubTokens(_ tokenizer: CFStringTokenizer!, _ ranges: UnsafeMutablePointer<CFRange>!, _ maxRangeLength: CFIndex, _ derivedSubTokens: CFMutableArray!) -> CFIndex
