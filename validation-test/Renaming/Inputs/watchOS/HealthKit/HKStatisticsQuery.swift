
@available(watchOS 2.0, *)
class HKStatisticsQuery : HKQuery {
  init(quantityType quantityType: HKQuantityType, quantitySamplePredicate quantitySamplePredicate: NSPredicate?, options options: HKStatisticsOptions = [], completionHandler handler: (HKStatisticsQuery, HKStatistics?, NSError?) -> Void)
}
