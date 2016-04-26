
@available(OSX 10.0, *)
let NSFontAttributeName: String
@available(OSX 10.0, *)
let NSParagraphStyleAttributeName: String
@available(OSX 10.0, *)
let NSForegroundColorAttributeName: String
@available(OSX 10.0, *)
let NSBackgroundColorAttributeName: String
@available(OSX 10.0, *)
let NSLigatureAttributeName: String
@available(OSX 10.0, *)
let NSKernAttributeName: String
@available(OSX 10.0, *)
let NSStrikethroughStyleAttributeName: String
@available(OSX 10.0, *)
let NSUnderlineStyleAttributeName: String
@available(OSX 10.0, *)
let NSStrokeColorAttributeName: String
@available(OSX 10.0, *)
let NSStrokeWidthAttributeName: String
@available(OSX 10.0, *)
let NSShadowAttributeName: String
@available(OSX 10.10, *)
let NSTextEffectAttributeName: String
@available(OSX 10.0, *)
let NSAttachmentAttributeName: String
@available(OSX 10.0, *)
let NSLinkAttributeName: String
@available(OSX 10.0, *)
let NSBaselineOffsetAttributeName: String
@available(OSX 10.0, *)
let NSUnderlineColorAttributeName: String
@available(OSX 10.0, *)
let NSStrikethroughColorAttributeName: String
@available(OSX 10.0, *)
let NSObliquenessAttributeName: String
@available(OSX 10.0, *)
let NSExpansionAttributeName: String
@available(OSX 10.6, *)
let NSWritingDirectionAttributeName: String
@available(OSX 10.7, *)
let NSVerticalGlyphFormAttributeName: String
let NSCursorAttributeName: String
let NSToolTipAttributeName: String
let NSMarkedClauseSegmentAttributeName: String
@available(OSX 10.8, *)
let NSTextAlternativesAttributeName: String
let NSSpellingStateAttributeName: String
let NSSuperscriptAttributeName: String
let NSGlyphInfoAttributeName: String
@available(OSX 10.0, *)
enum NSUnderlineStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case styleNone
  case styleSingle
  @available(OSX 10.0, *)
  case styleThick
  @available(OSX 10.0, *)
  case styleDouble
  @available(OSX 10.0, *)
  static var patternSolid: NSUnderlineStyle { get }
  @available(OSX 10.0, *)
  case patternDot
  @available(OSX 10.0, *)
  case patternDash
  @available(OSX 10.0, *)
  case patternDashDot
  @available(OSX 10.0, *)
  case patternDashDotDot
  @available(OSX 10.0, *)
  case byWord
}
@available(OSX 10.11, *)
enum NSWritingDirectionFormatType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case embedding
  case override
}
@available(OSX 10.10, *)
let NSTextEffectLetterpressStyle: String
@available(OSX 10.5, *)
var NSSpellingStateSpellingFlag: Int { get }
@available(OSX 10.5, *)
var NSSpellingStateGrammarFlag: Int { get }
extension NSMutableAttributedString {
  @available(OSX 10.0, *)
  func fixAttributes(in range: NSRange)
  func fixFontAttribute(in range: NSRange)
  func fixParagraphStyleAttribute(in range: NSRange)
  func fixAttachmentAttribute(in range: NSRange)
}
@available(OSX 10.0, *)
let NSPlainTextDocumentType: String
@available(OSX 10.0, *)
let NSRTFTextDocumentType: String
@available(OSX 10.0, *)
let NSRTFDTextDocumentType: String
@available(OSX 10.0, *)
let NSHTMLTextDocumentType: String
let NSMacSimpleTextDocumentType: String
let NSDocFormatTextDocumentType: String
let NSWordMLTextDocumentType: String
let NSWebArchiveTextDocumentType: String
@available(OSX 10.5, *)
let NSOfficeOpenXMLTextDocumentType: String
@available(OSX 10.5, *)
let NSOpenDocumentTextDocumentType: String
@available(OSX 10.7, *)
let NSTextLayoutSectionOrientation: String
@available(OSX 10.7, *)
let NSTextLayoutSectionRange: String
@available(OSX 10.0, *)
let NSDocumentTypeDocumentAttribute: String
let NSConvertedDocumentAttribute: String
let NSCocoaVersionDocumentAttribute: String
@available(OSX 10.6, *)
let NSFileTypeDocumentAttribute: String
let NSTitleDocumentAttribute: String
let NSCompanyDocumentAttribute: String
let NSCopyrightDocumentAttribute: String
let NSSubjectDocumentAttribute: String
let NSAuthorDocumentAttribute: String
let NSKeywordsDocumentAttribute: String
let NSCommentDocumentAttribute: String
let NSEditorDocumentAttribute: String
let NSCreationTimeDocumentAttribute: String
let NSModificationTimeDocumentAttribute: String
@available(OSX 10.5, *)
let NSManagerDocumentAttribute: String
@available(OSX 10.6, *)
let NSCategoryDocumentAttribute: String
@available(OSX 10.0, *)
let NSCharacterEncodingDocumentAttribute: String
@available(OSX 10.11, *)
let NSDefaultAttributesDocumentAttribute: String
@available(OSX 10.0, *)
let NSPaperSizeDocumentAttribute: String
let NSLeftMarginDocumentAttribute: String
let NSRightMarginDocumentAttribute: String
let NSTopMarginDocumentAttribute: String
let NSBottomMarginDocumentAttribute: String
@available(OSX 10.0, *)
let NSViewSizeDocumentAttribute: String
@available(OSX 10.0, *)
let NSViewZoomDocumentAttribute: String
@available(OSX 10.0, *)
let NSViewModeDocumentAttribute: String
@available(OSX 10.0, *)
let NSReadOnlyDocumentAttribute: String
@available(OSX 10.0, *)
let NSBackgroundColorDocumentAttribute: String
@available(OSX 10.0, *)
let NSHyphenationFactorDocumentAttribute: String
@available(OSX 10.0, *)
let NSDefaultTabIntervalDocumentAttribute: String
@available(OSX 10.7, *)
let NSTextLayoutSectionsAttribute: String
let NSExcludedElementsDocumentAttribute: String
let NSTextEncodingNameDocumentAttribute: String
let NSPrefixSpacesDocumentAttribute: String
let NSDocumentTypeDocumentOption: String
let NSDefaultAttributesDocumentOption: String
let NSCharacterEncodingDocumentOption: String
let NSTextEncodingNameDocumentOption: String
let NSBaseURLDocumentOption: String
let NSTimeoutDocumentOption: String
let NSWebPreferencesDocumentOption: String
let NSWebResourceLoadDelegateDocumentOption: String
let NSTextSizeMultiplierDocumentOption: String
@available(OSX 10.6, *)
let NSFileTypeDocumentOption: String
extension NSAttributedString {
  @available(OSX 10.11, *)
  init(url url: NSURL, options options: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(OSX 10.0, *)
  init(data data: NSData, options options: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(OSX 10.0, *)
  @discardableResult
  func data(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) throws -> NSData
  @available(OSX 10.0, *)
  @discardableResult
  func fileWrapper(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) throws -> NSFileWrapper
  init?(rtf data: NSData, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(rtfd data: NSData, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(html data: NSData, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(html data: NSData, baseURL base: NSURL, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(docFormat data: NSData, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(html data: NSData, options options: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  init?(rtfdFileWrapper wrapper: NSFileWrapper, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  @discardableResult
  func rtf(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) -> NSData?
  @discardableResult
  func rtfd(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) -> NSData?
  @discardableResult
  func rtfdFileWrapper(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) -> NSFileWrapper?
  @discardableResult
  func docFormat(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) -> NSData?
}
extension NSMutableAttributedString {
  @available(OSX 10.11, *)
  func read(from url: NSURL, options opts: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?, error error: ()) throws
  @available(OSX 10.0, *)
  func read(from data: NSData, options opts: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?, error error: ()) throws
}
extension NSAttributedString {
  @discardableResult
  func fontAttributes(in range: NSRange) -> [String : AnyObject]
  @discardableResult
  func rulerAttributes(in range: NSRange) -> [String : AnyObject]
  @available(OSX 10.11, *)
  @discardableResult
  func containsAttachments(in range: NSRange) -> Bool
  @discardableResult
  func lineBreak(before location: Int, within aRange: NSRange) -> Int
  @discardableResult
  func lineBreakByHyphenating(before location: Int, within aRange: NSRange) -> Int
  @discardableResult
  func doubleClick(at location: Int) -> NSRange
  @discardableResult
  func nextWord(from location: Int, forward isForward: Bool) -> Int
  @discardableResult
  func range(of block: NSTextBlock, at location: Int) -> NSRange
  @discardableResult
  func range(of table: NSTextTable, at location: Int) -> NSRange
  @discardableResult
  func range(of list: NSTextList, at location: Int) -> NSRange
  @discardableResult
  func itemNumber(in list: NSTextList, at location: Int) -> Int
}
extension NSAttributedString : NSPasteboardReading, NSPasteboardWriting {
  @available(OSX 10.5, *)
  @discardableResult
  class func textTypes() -> [String]
  @available(OSX 10.5, *)
  @discardableResult
  class func textUnfilteredTypes() -> [String]
}
extension NSMutableAttributedString {
  func superscriptRange(_ range: NSRange)
  func subscriptRange(_ range: NSRange)
  func unscriptRange(_ range: NSRange)
  func applyFontTraits(_ traitMask: NSFontTraitMask, range range: NSRange)
  func setAlignment(_ alignment: NSTextAlignment, range range: NSRange)
  func setBaseWritingDirection(_ writingDirection: NSWritingDirection, range range: NSRange)
}
@available(OSX, introduced: 10.0, deprecated: 10.11, message: "This attribute is bound to a specific implementation of ATS feature and not generically supported by wide range of fonts. The majority of characters accessed through this API are now encoded in the Unicode standard. Use the CTFont feature API for fine control over character shape choices.")
let NSCharacterShapeAttributeName: String
@available(OSX, introduced: 10.8, deprecated: 10.11)
let NSUsesScreenFontsDocumentAttribute: String
@available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use NSUnderlineByWord instead")
var NSUnderlineByWordMask: Int
extension NSAttributedString {
  var containsAttachments: Bool { get }
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -initWithURL:options:documentAttributes:error: instead")
  init?(url url: NSURL, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -initWithURL:options:documentAttributes:error: instead")
  init?(path path: String, documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use NSDataDetector instead")
  @discardableResult
  func url(at location: Int, effectiveRange effectiveRange: NSRangePointer) -> NSURL?
}
extension NSMutableAttributedString {
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -readFromURL:options:documentAttributes:error: instead")
  @discardableResult
  func read(from url: NSURL, options options: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> Bool
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use -readFromData:options:documentAttributes:error: instead")
  @discardableResult
  func read(from data: NSData, options options: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> Bool
}
