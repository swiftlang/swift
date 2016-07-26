
class NSClassDescription : NSObject {
  class func register(_ description: NSClassDescription, for aClass: AnyClass)
  class func invalidateClassDescriptionCache()
  /*not inherited*/ init?(for aClass: AnyClass)
}
extension NSObject {
  @NSCopying var classDescription: NSClassDescription { get }
  var attributeKeys: [String] { get }
  var toOneRelationshipKeys: [String] { get }
  var toManyRelationshipKeys: [String] { get }
  @discardableResult
  class func inverse(forRelationshipKey relationshipKey: String) -> String?
  @discardableResult
  func inverse(forRelationshipKey relationshipKey: String) -> String?
  class func classDescription() -> NSClassDescription
  class func attributeKeys() -> [String]
  class func toOneRelationshipKeys() -> [String]
  class func toManyRelationshipKeys() -> [String]
}
let NSClassDescriptionNeededForClassNotification: String
