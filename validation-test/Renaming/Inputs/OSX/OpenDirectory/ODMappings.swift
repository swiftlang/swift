
@available(OSX 10.9, *)
class ODMappings : NSObject {
  @available(OSX 10.9, *)
  var comment: String!
  @available(OSX 10.9, *)
  var templateName: String!
  @available(OSX 10.9, *)
  var identifier: String!
  @available(OSX 10.9, *)
  var recordTypes: [AnyObject]! { get }
  @available(OSX 10.9, *)
  var function: String!
  @available(OSX 10.9, *)
  var functionAttributes: [AnyObject]!
  @available(OSX 10.9, *)
  @discardableResult
  func recordMap(forStandardRecordType stdType: String!) -> ODRecordMap!
  @available(OSX 10.9, *)
  func setRecordMap(_ map: ODRecordMap!, forStandardRecordType stdType: String!)
}
