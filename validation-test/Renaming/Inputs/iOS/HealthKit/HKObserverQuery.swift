
typealias HKObserverQueryCompletionHandler = () -> Void
@available(iOS 8.0, *)
class HKObserverQuery : HKQuery {
  init(sampleType sampleType: HKSampleType, predicate predicate: NSPredicate?, updateHandler updateHandler: (HKObserverQuery, HKObserverQueryCompletionHandler, NSError?) -> Void)
}
