
@available(watchOS 2.0, *)
class HKStatisticsCollection : NSObject {
  @discardableResult
  func statistics(for date: NSDate) -> HKStatistics?
  func enumerateStatistics(from startDate: NSDate, to endDate: NSDate, with block: (HKStatistics, UnsafeMutablePointer<ObjCBool>) -> Void)
  @discardableResult
  func statistics() -> [HKStatistics]
  @discardableResult
  func sources() -> Set<HKSource>
}
@available(watchOS 2.0, *)
class HKStatisticsCollectionQuery : HKQuery {
  var anchorDate: NSDate { get }
  var options: HKStatisticsOptions { get }
  @NSCopying var intervalComponents: NSDateComponents { get }
  var initialResultsHandler: ((HKStatisticsCollectionQuery, HKStatisticsCollection?, NSError?) -> Void)?
  var statisticsUpdateHandler: ((HKStatisticsCollectionQuery, HKStatistics?, HKStatisticsCollection?, NSError?) -> Void)?
  init(quantityType quantityType: HKQuantityType, quantitySamplePredicate quantitySamplePredicate: NSPredicate?, options options: HKStatisticsOptions = [], anchorDate anchorDate: NSDate, intervalComponents intervalComponents: NSDateComponents)
}
