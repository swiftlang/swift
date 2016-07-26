
@available(watchOS 2.2, *)
class HKActivitySummaryQuery : HKQuery {
  var updateHandler: ((HKActivitySummaryQuery, [HKActivitySummary]?, NSError?) -> Void)?
  init(predicate predicate: NSPredicate?, resultsHandler handler: (HKActivitySummaryQuery, [HKActivitySummary]?, NSError?) -> Void)
}
