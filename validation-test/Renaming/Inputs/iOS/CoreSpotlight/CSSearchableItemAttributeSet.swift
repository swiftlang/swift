
@available(iOS 9.0, *)
class CSSearchableItemAttributeSet : NSObject, NSCopying, NSSecureCoding {
  init(itemContentType itemContentType: String)
}
class CSLocalizedString : NSString {
  init(localizedStrings localizedStrings: [NSObject : AnyObject])
  @discardableResult
  func localizedString() -> String
}
@available(iOS 9.0, *)
class CSCustomAttributeKey : NSObject, NSCopying, NSSecureCoding {
  convenience init?(keyName keyName: String)
  init?(keyName keyName: String, searchable searchable: Bool, searchableByDefault searchableByDefault: Bool, unique unique: Bool, multiValued multiValued: Bool)
  var keyName: String { get }
  var isSearchable: Bool { get }
  var isSearchableByDefault: Bool { get }
  var isUnique: Bool { get }
  var isMultiValued: Bool { get }
}
extension CSSearchableItemAttributeSet {
  func setValue(_ value: NSSecureCoding?, forCustomKey key: CSCustomAttributeKey)
  @discardableResult
  func value(forCustomKey key: CSCustomAttributeKey) -> NSSecureCoding?
}
extension NSUserActivity {
  @available(iOS 9.0, *)
  @NSCopying var contentAttributeSet: CSSearchableItemAttributeSet?
}
