
@available(iOS 8.0, *)
class HKSourceQuery : HKQuery {
  init(sampleType sampleType: HKSampleType, samplePredicate objectPredicate: NSPredicate?, completionHandler completionHandler: (HKSourceQuery, Set<HKSource>?, NSError?) -> Void)
}
