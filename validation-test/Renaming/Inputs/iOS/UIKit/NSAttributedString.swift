
@available(iOS 6.0, *)
let NSFontAttributeName: String
@available(iOS 6.0, *)
let NSParagraphStyleAttributeName: String
@available(iOS 6.0, *)
let NSForegroundColorAttributeName: String
@available(iOS 6.0, *)
let NSBackgroundColorAttributeName: String
@available(iOS 6.0, *)
let NSLigatureAttributeName: String
@available(iOS 6.0, *)
let NSKernAttributeName: String
@available(iOS 6.0, *)
let NSStrikethroughStyleAttributeName: String
@available(iOS 6.0, *)
let NSUnderlineStyleAttributeName: String
@available(iOS 6.0, *)
let NSStrokeColorAttributeName: String
@available(iOS 6.0, *)
let NSStrokeWidthAttributeName: String
@available(iOS 6.0, *)
let NSShadowAttributeName: String
@available(iOS 7.0, *)
let NSTextEffectAttributeName: String
@available(iOS 7.0, *)
let NSAttachmentAttributeName: String
@available(iOS 7.0, *)
let NSLinkAttributeName: String
@available(iOS 7.0, *)
let NSBaselineOffsetAttributeName: String
@available(iOS 7.0, *)
let NSUnderlineColorAttributeName: String
@available(iOS 7.0, *)
let NSStrikethroughColorAttributeName: String
@available(iOS 7.0, *)
let NSObliquenessAttributeName: String
@available(iOS 7.0, *)
let NSExpansionAttributeName: String
@available(iOS 7.0, *)
let NSWritingDirectionAttributeName: String
@available(iOS 6.0, *)
let NSVerticalGlyphFormAttributeName: String
@available(iOS 6.0, *)
enum NSUnderlineStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case styleNone
  case styleSingle
  @available(iOS 7.0, *)
  case styleThick
  @available(iOS 7.0, *)
  case styleDouble
  @available(iOS 7.0, *)
  static var patternSolid: NSUnderlineStyle { get }
  @available(iOS 7.0, *)
  case patternDot
  @available(iOS 7.0, *)
  case patternDash
  @available(iOS 7.0, *)
  case patternDashDot
  @available(iOS 7.0, *)
  case patternDashDotDot
  @available(iOS 7.0, *)
  case byWord
}
@available(iOS 9.0, *)
enum NSWritingDirectionFormatType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case embedding
  case override
}
@available(iOS 7.0, *)
let NSTextEffectLetterpressStyle: String
extension NSMutableAttributedString {
  @available(iOS 7.0, *)
  func fixAttributes(in range: NSRange)
}
@available(iOS 7.0, *)
let NSPlainTextDocumentType: String
@available(iOS 7.0, *)
let NSRTFTextDocumentType: String
@available(iOS 7.0, *)
let NSRTFDTextDocumentType: String
@available(iOS 7.0, *)
let NSHTMLTextDocumentType: String
@available(iOS 7.0, *)
let NSTextLayoutSectionOrientation: String
@available(iOS 7.0, *)
let NSTextLayoutSectionRange: String
@available(iOS 7.0, *)
let NSDocumentTypeDocumentAttribute: String
@available(iOS 7.0, *)
let NSCharacterEncodingDocumentAttribute: String
@available(iOS 7.0, *)
let NSDefaultAttributesDocumentAttribute: String
@available(iOS 7.0, *)
let NSPaperSizeDocumentAttribute: String
@available(iOS 7.0, *)
let NSPaperMarginDocumentAttribute: String
@available(iOS 7.0, *)
let NSViewSizeDocumentAttribute: String
@available(iOS 7.0, *)
let NSViewZoomDocumentAttribute: String
@available(iOS 7.0, *)
let NSViewModeDocumentAttribute: String
@available(iOS 7.0, *)
let NSReadOnlyDocumentAttribute: String
@available(iOS 7.0, *)
let NSBackgroundColorDocumentAttribute: String
@available(iOS 7.0, *)
let NSHyphenationFactorDocumentAttribute: String
@available(iOS 7.0, *)
let NSDefaultTabIntervalDocumentAttribute: String
@available(iOS 7.0, *)
let NSTextLayoutSectionsAttribute: String
extension NSAttributedString {
  @available(iOS 9.0, *)
  init(url url: NSURL, options options: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(iOS 7.0, *)
  init(data data: NSData, options options: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(iOS 7.0, *)
  @discardableResult
  func data(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) throws -> NSData
  @available(iOS 7.0, *)
  @discardableResult
  func fileWrapper(from range: NSRange, documentAttributes dict: [String : AnyObject] = [:]) throws -> NSFileWrapper
}
extension NSMutableAttributedString {
  @available(iOS 9.0, *)
  func read(from url: NSURL, options opts: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
  @available(iOS 7.0, *)
  func read(from data: NSData, options opts: [String : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
}
extension NSAttributedString {
  @available(iOS 9.0, *)
  @discardableResult
  func containsAttachments(in range: NSRange) -> Bool
}
@available(iOS, introduced: 7.0, deprecated: 9.0, message: "Use NSWritingDirectionFormatType instead")
enum NSTextWritingDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case embedding
  case override
}
extension NSAttributedString {
  @available(iOS, introduced: 7.0, deprecated: 9.0, message: "Use -initWithURL:options:documentAttributes:error: instead")
  init(fileURL url: NSURL, options options: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
}
extension NSMutableAttributedString {
  @available(iOS, introduced: 7.0, deprecated: 9.0, message: "Use -readFromURL:options:documentAttributes:error: instead")
  func read(fromFileURL url: NSURL, options opts: [NSObject : AnyObject] = [:], documentAttributes dict: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) throws
}
