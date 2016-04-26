
let HKObjectQueryNoLimit: Int
@available(iOS 8.0, *)
class HKSampleQuery : HKQuery {
  var limit: Int { get }
  var sortDescriptors: [NSSortDescriptor]? { get }
  init(sampleType sampleType: HKSampleType, predicate predicate: NSPredicate?, limit limit: Int, sortDescriptors sortDescriptors: [NSSortDescriptor]?, resultsHandler resultsHandler: (HKSampleQuery, [HKSample]?, NSError?) -> Void)
}
