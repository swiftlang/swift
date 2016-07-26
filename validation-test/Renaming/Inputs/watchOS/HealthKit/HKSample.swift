
@available(watchOS 2.0, *)
class HKSample : HKObject {
  var sampleType: HKSampleType { get }
  var startDate: NSDate { get }
  var endDate: NSDate { get }
}
@available(watchOS 2.0, *)
let HKSampleSortIdentifierStartDate: String
@available(watchOS 2.0, *)
let HKSampleSortIdentifierEndDate: String
@available(watchOS 2.0, *)
let HKPredicateKeyPathStartDate: String
@available(watchOS 2.0, *)
let HKPredicateKeyPathEndDate: String
