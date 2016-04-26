
let HKObjectQueryNoLimit: Int
@available(watchOS 2.0, *)
class HKSampleQuery : HKQuery {
  var limit: Int { get }
  var sortDescriptors: [NSSortDescriptor]? { get }
  init(sampleType sampleType: HKSampleType, predicate predicate: NSPredicate?, limit limit: Int, sortDescriptors sortDescriptors: [NSSortDescriptor]?, resultsHandler resultsHandler: (HKSampleQuery, [HKSample]?, NSError?) -> Void)
}
