
var HKAnchoredObjectQueryNoAnchor: Int32 { get }
@available(iOS 9.0, *)
class HKQueryAnchor : NSObject, NSSecureCoding, NSCopying {
  convenience init(fromValue value: Int)
}
@available(iOS 8.0, *)
class HKAnchoredObjectQuery : HKQuery {
  @available(iOS 9.0, *)
  var updateHandler: ((HKAnchoredObjectQuery, [HKSample]?, [HKDeletedObject]?, HKQueryAnchor?, NSError?) -> Void)?
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  init(type type: HKSampleType, predicate predicate: NSPredicate?, anchor anchor: Int, limit limit: Int, completionHandler handler: (HKAnchoredObjectQuery, [HKSample]?, Int, NSError?) -> Void)
  @available(iOS 9.0, *)
  init(type type: HKSampleType, predicate predicate: NSPredicate?, anchor anchor: HKQueryAnchor?, limit limit: Int, resultsHandler handler: (HKAnchoredObjectQuery, [HKSample]?, [HKDeletedObject]?, HKQueryAnchor?, NSError?) -> Void)
}
