
@available(OSX 10.9, *)
class ODRecordMap : NSObject {
  @available(OSX 10.9, *)
  var native: String!
  @available(OSX 10.9, *)
  var odPredicate: [NSObject : AnyObject]!
  @available(OSX 10.9, *)
  var attributes: [NSObject : AnyObject]! { get }
  @available(OSX 10.9, *)
  var standardAttributeTypes: [AnyObject]! { get }
  @available(OSX 10.9, *)
  @discardableResult
  func attributeMap(forStandardAttribute standardAttribute: String!) -> ODAttributeMap!
  @available(OSX 10.9, *)
  func setAttributeMap(_ attributeMap: ODAttributeMap!, forStandardAttribute standardAttribute: String!)
}
