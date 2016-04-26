
@available(OSX 10.10, *)
class NSExtensionItem : NSObject, NSCopying, NSSecureCoding {
  @NSCopying var attributedTitle: NSAttributedString?
  @NSCopying var attributedContentText: NSAttributedString?
  var attachments: [AnyObject]?
  var userInfo: [NSObject : AnyObject]?
}
@available(OSX 10.10, *)
let NSExtensionItemAttributedTitleKey: String
@available(OSX 10.10, *)
let NSExtensionItemAttributedContentTextKey: String
@available(OSX 10.10, *)
let NSExtensionItemAttachmentsKey: String
