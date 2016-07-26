
@available(iOS 7.0, *)
var NSAttachmentCharacter: Int { get }
protocol NSTextAttachmentContainer : NSObjectProtocol {
  @available(iOS 7.0, *)
  @discardableResult
  func image(forBounds imageBounds: CGRect, textContainer textContainer: NSTextContainer?, characterIndex charIndex: Int) -> UIImage?
  @available(iOS 7.0, *)
  @discardableResult
  func attachmentBounds(for textContainer: NSTextContainer?, proposedLineFragment lineFrag: CGRect, glyphPosition position: CGPoint, characterIndex charIndex: Int) -> CGRect
}
@available(iOS 7.0, *)
class NSTextAttachment : NSObject, NSTextAttachmentContainer, NSCoding {
  @available(iOS 7.0, *)
  init(data contentData: NSData?, ofType uti: String?)
  @available(iOS 7.0, *)
  @NSCopying var contents: NSData?
  @available(iOS 7.0, *)
  var fileType: String?
  @available(iOS 7.0, *)
  var image: UIImage?
  @available(iOS 7.0, *)
  var bounds: CGRect
  var fileWrapper: NSFileWrapper?
}
extension NSAttributedString {
  @available(iOS 7.0, *)
  /*not inherited*/ init(attachment attachment: NSTextAttachment)
}
