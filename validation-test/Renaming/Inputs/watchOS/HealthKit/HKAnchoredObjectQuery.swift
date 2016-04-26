
var HKAnchoredObjectQueryNoAnchor: Int32 { get }
@available(watchOS 2.0, *)
class HKQueryAnchor : NSObject, NSSecureCoding, NSCopying {
  convenience init(fromValue value: Int)
}
@available(watchOS 2.0, *)
class HKAnchoredObjectQuery : HKQuery {
  @available(watchOS 2.0, *)
  var updateHandler: ((HKAnchoredObjectQuery, [HKSample]?, [HKDeletedObject]?, HKQueryAnchor?, NSError?) -> Void)?
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  init(type type: HKSampleType, predicate predicate: NSPredicate?, anchor anchor: Int, limit limit: Int, completionHandler handler: (HKAnchoredObjectQuery, [HKSample]?, Int, NSError?) -> Void)
  @available(watchOS 2.0, *)
  init(type type: HKSampleType, predicate predicate: NSPredicate?, anchor anchor: HKQueryAnchor?, limit limit: Int, resultsHandler handler: (HKAnchoredObjectQuery, [HKSample]?, [HKDeletedObject]?, HKQueryAnchor?, NSError?) -> Void)
}
