
@available(watchOS 2.0, *)
let NSFontAttributeName: String
@available(watchOS 2.0, *)
let NSParagraphStyleAttributeName: String
@available(watchOS 2.0, *)
let NSForegroundColorAttributeName: String
@available(watchOS 2.0, *)
let NSBackgroundColorAttributeName: String
@available(watchOS 2.0, *)
let NSLigatureAttributeName: String
@available(watchOS 2.0, *)
let NSKernAttributeName: String
@available(watchOS 2.0, *)
let NSStrikethroughStyleAttributeName: String
@available(watchOS 2.0, *)
let NSUnderlineStyleAttributeName: String
@available(watchOS 2.0, *)
let NSStrokeColorAttributeName: String
@available(watchOS 2.0, *)
let NSStrokeWidthAttributeName: String
@available(watchOS 2.0, *)
let NSShadowAttributeName: String
@available(watchOS 2.0, *)
let NSTextEffectAttributeName: String
@available(watchOS 2.0, *)
let NSAttachmentAttributeName: String
@available(watchOS 2.0, *)
let NSLinkAttributeName: String
@available(watchOS 2.0, *)
let NSBaselineOffsetAttributeName: String
@available(watchOS 2.0, *)
let NSUnderlineColorAttributeName: String
@available(watchOS 2.0, *)
let NSStrikethroughColorAttributeName: String
@available(watchOS 2.0, *)
let NSObliquenessAttributeName: String
@available(watchOS 2.0, *)
let NSExpansionAttributeName: String
@available(watchOS 2.0, *)
let NSWritingDirectionAttributeName: String
@available(watchOS 2.0, *)
let NSVerticalGlyphFormAttributeName: String
@available(watchOS 2.0, *)
enum NSUnderlineStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case styleNone
  case styleSingle
  @available(watchOS 2.0, *)
  case styleThick
  @available(watchOS 2.0, *)
  case styleDouble
  @available(watchOS 2.0, *)
  static var patternSolid: NSUnderlineStyle { get }
  @available(watchOS 2.0, *)
  case patternDot
  @available(watchOS 2.0, *)
  case patternDash
  @available(watchOS 2.0, *)
  case patternDashDot
  @available(watchOS 2.0, *)
  case patternDashDotDot
  @available(watchOS 2.0, *)
  case byWord
}
@available(watchOS 2.0, *)
enum NSWritingDirectionFormatType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case embedding
  case override
}
@available(watchOS 2.0, *)
let NSTextEffectLetterpressStyle: String
extension NSMutableAttributedString {
  @available(watchOS 2.0, *)
  func fixAttributes(in range: NSRange)
}
@available(watchOS 2.0, *)
let NSPlainTextDocumentType: String
@available(watchOS 2.0, *)
let NSRTFTextDocumentType: String
@available(watchOS 2.0, *)
let NSRTFDTextDocumentType: String
@available(watchOS 2.0, *)
let NSHTMLTextDocumentType: String
@available(watchOS 2.0, *)
let NSTextLayoutSectionOrientation: String
@available(watchOS 2.0, *)
let NSTextLayoutSectionRange: String
@available(watchOS 2.0, *)
let NSDocumentTypeDocumentAttribute: String
@available(watchOS 2.0, *)
let NSCharacterEncodingDocumentAttribute: String
@available(watchOS 2.0, *)
let NSDefaultAttributesDocumentAttribute: String
@available(watchOS 2.0, *)
let NSPaperSizeDocumentAttribute: String
@available(watchOS 2.0, *)
let NSPaperMarginDocumentAttribute: String
@available(watchOS 2.0, *)
let NSViewSizeDocumentAttribute: String
@available(watchOS 2.0, *)
let NSViewZoomDocumentAttribute: String
@available(watchOS 2.0, *)
let NSViewModeDocumentAttribute: String
@available(watchOS 2.0, *)
let NSReadOnlyDocumentAttribute: String
@available(watchOS 2.0, *)
let NSBackgroundColorDocumentAttribute: String
@available(watchOS 2.0, *)
let NSHyphenationFactorDocumentAttribute: String
@available(watchOS 2.0, *)
let NSDefaultTabIntervalDocumentAttribute: String
@available(watchOS 2.0, *)
let NSTextLayoutSectionsAttribute: String
extension NSAttributedString {
  @available(watchOS 2.0, *)
  init(url url: NSURL, options options: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(watchOS 2.0, *)
  init(data data: NSData, options options: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(watchOS 2.0, *)
  @discardableResult
  func data(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) throws -> NSData
  @available(watchOS 2.0, *)
  @discardableResult
  func fileWrapper(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) throws -> NSFileWrapper
}
extension NSMutableAttributedString {
  @available(watchOS 2.0, *)
  func read(from url: NSURL, options opts: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(watchOS 2.0, *)
  func read(from data: NSData, options opts: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
}
extension NSAttributedString {
  @available(watchOS 2.0, *)
  @discardableResult
  func containsAttachments(in range: NSRange) -> Bool
}
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSWritingDirectionFormatType instead")
enum NSTextWritingDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case embedding
  case override
}
extension NSAttributedString {
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use -initWithURL:options:documentAttributes:error: instead")
  init(fileURL url: NSURL, options options: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
}
extension NSMutableAttributedString {
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use -readFromURL:options:documentAttributes:error: instead")
  func read(fromFileURL url: NSURL, options opts: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
}
