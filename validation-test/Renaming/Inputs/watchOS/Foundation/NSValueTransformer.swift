
@available(watchOS 2.0, *)
let NSNegateBooleanTransformerName: String
@available(watchOS 2.0, *)
let NSIsNilTransformerName: String
@available(watchOS 2.0, *)
let NSIsNotNilTransformerName: String
@available(watchOS 2.0, *)
let NSUnarchiveFromDataTransformerName: String
@available(watchOS 2.0, *)
let NSKeyedUnarchiveFromDataTransformerName: String
@available(watchOS 2.0, *)
class NSValueTransformer : NSObject {
  class func setValueTransformer(_ transformer: NSValueTransformer?, forName name: String)
  /*not inherited*/ init?(forName name: String)
  @discardableResult
  class func valueTransformerNames() -> [String]
  @discardableResult
  class func transformedValueClass() -> AnyClass
  @discardableResult
  class func allowsReverseTransformation() -> Bool
  @discardableResult
  func transformedValue(_ value: AnyObject?) -> AnyObject?
  @discardableResult
  func reverseTransformedValue(_ value: AnyObject?) -> AnyObject?
}
