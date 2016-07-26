
@available(tvOS 8.0, *)
class NSExtensionItem : NSObject, NSCopying, NSSecureCoding {
  @NSCopying var attributedTitle: NSAttributedString?
  @NSCopying var attributedContentText: NSAttributedString?
  var attachments: [AnyObject]?
  var userInfo: [NSObject : AnyObject]?
}
@available(tvOS 8.0, *)
let NSExtensionItemAttributedTitleKey: String
@available(tvOS 8.0, *)
let NSExtensionItemAttributedContentTextKey: String
@available(tvOS 8.0, *)
let NSExtensionItemAttachmentsKey: String
