
@available(iOS 9.0, *)
let CSMailboxInbox: String
@available(iOS 9.0, *)
let CSMailboxDrafts: String
@available(iOS 9.0, *)
let CSMailboxSent: String
@available(iOS 9.0, *)
let CSMailboxJunk: String
@available(iOS 9.0, *)
let CSMailboxTrash: String
@available(iOS 9.0, *)
let CSMailboxArchive: String
extension CSSearchableItemAttributeSet {
  var accountIdentifier: String?
  var accountHandles: [String]?
  @NSCopying var htmlContentData: NSData?
  var textContent: String?
  var authors: [CSPerson]?
  var primaryRecipients: [CSPerson]?
  var additionalRecipients: [CSPerson]?
  var hiddenAdditionalRecipients: [CSPerson]?
  var emailHeaders: [String : [AnyObject]]?
  var mailboxIdentifiers: [String]?
  var authorNames: [String]?
  var recipientNames: [String]?
  var authorEmailAddresses: [String]?
  var recipientEmailAddresses: [String]?
  var authorAddresses: [String]?
  var recipientAddresses: [String]?
  var phoneNumbers: [String]?
  var emailAddresses: [String]?
  var instantMessageAddresses: [String]?
  var likelyJunk: NSNumber
}
