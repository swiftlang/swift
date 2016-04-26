
@available(watchOS 2.0, *)
class HKCategorySample : HKSample {
  var categoryType: HKCategoryType { get }
  var value: Int { get }
  convenience init(type type: HKCategoryType, value value: Int, start startDate: NSDate, end endDate: NSDate, metadata metadata: [String : AnyObject]?)
  convenience init(type type: HKCategoryType, value value: Int, start startDate: NSDate, end endDate: NSDate)
  @available(watchOS 2.0, *)
  convenience init(type type: HKCategoryType, value value: Int, start startDate: NSDate, end endDate: NSDate, device device: HKDevice?, metadata metadata: [String : AnyObject]?)
}
@available(watchOS 2.0, *)
let HKPredicateKeyPathCategoryValue: String
