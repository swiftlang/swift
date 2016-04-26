
@available(iOS 9.0, *)
let CSSearchableItemActionType: String
@available(iOS 9.0, *)
let CSSearchableItemActivityIdentifier: String
@available(iOS 9.0, *)
class CSSearchableItem : NSObject, NSSecureCoding, NSCopying {
  init(uniqueIdentifier uniqueIdentifier: String?, domainIdentifier domainIdentifier: String?, attributeSet attributeSet: CSSearchableItemAttributeSet)
  var uniqueIdentifier: String
  var domainIdentifier: String?
  @NSCopying var expirationDate: NSDate!
  var attributeSet: CSSearchableItemAttributeSet
}
