
@available(iOS 8.0, *)
class HKHealthStore : NSObject {
  @discardableResult
  class func isHealthDataAvailable() -> Bool
  @discardableResult
  func authorizationStatus(for type: HKObjectType) -> HKAuthorizationStatus
  func requestAuthorization(toShare typesToShare: Set<HKSampleType>?, read typesToRead: Set<HKObjectType>?, completion completion: (Bool, NSError?) -> Void)
  @available(iOS 9.0, *)
  func handleAuthorizationForExtension(completion completion: (Bool, NSError?) -> Void)
  @available(iOS 9.0, *)
  @discardableResult
  func earliestPermittedSampleDate() -> NSDate
  func save(_ object: HKObject, withCompletion completion: (Bool, NSError?) -> Void)
  func save(_ objects: [HKObject], withCompletion completion: (Bool, NSError?) -> Void)
  func delete(_ object: HKObject, withCompletion completion: (Bool, NSError?) -> Void)
  @available(iOS 9.0, *)
  func delete(_ objects: [HKObject], withCompletion completion: (Bool, NSError?) -> Void)
  @available(iOS 9.0, *)
  func deleteObjects(of objectType: HKObjectType, predicate predicate: NSPredicate, withCompletion completion: (Bool, Int, NSError?) -> Void)
  func execute(_ query: HKQuery)
  func stop(_ query: HKQuery)
  @available(iOS 9.0, *)
  func splitTotalEnergy(_ totalEnergy: HKQuantity, start startDate: NSDate, end endDate: NSDate, resultsHandler resultsHandler: (HKQuantity?, HKQuantity?, NSError?) -> Void)
  @discardableResult
  func dateOfBirth() throws -> NSDate
  @discardableResult
  func biologicalSex() throws -> HKBiologicalSexObject
  @discardableResult
  func bloodType() throws -> HKBloodTypeObject
  @available(iOS 9.0, *)
  @discardableResult
  func fitzpatrickSkinType() throws -> HKFitzpatrickSkinTypeObject
}
extension HKHealthStore {
  func add(_ samples: [HKSample], to workout: HKWorkout, completion completion: (Bool, NSError?) -> Void)
}
extension HKHealthStore {
  func enableBackgroundDelivery(for type: HKObjectType, frequency frequency: HKUpdateFrequency, withCompletion completion: (Bool, NSError?) -> Void)
  func disableBackgroundDelivery(for type: HKObjectType, withCompletion completion: (Bool, NSError?) -> Void)
  func disableAllBackgroundDelivery(completion completion: (Bool, NSError?) -> Void)
}
@available(iOS 8.2, *)
let HKUserPreferencesDidChangeNotification: String
extension HKHealthStore {
  @available(iOS 8.2, *)
  func preferredUnits(for quantityTypes: Set<HKQuantityType>, completion completion: ([HKQuantityType : HKUnit], NSError?) -> Void)
}
@available(iOS 8.0, *)
class HKBiologicalSexObject : NSObject, NSCopying, NSSecureCoding {
  var biologicalSex: HKBiologicalSex { get }
}
@available(iOS 8.0, *)
class HKBloodTypeObject : NSObject, NSCopying, NSSecureCoding {
  var bloodType: HKBloodType { get }
}
@available(iOS 9.0, *)
class HKFitzpatrickSkinTypeObject : NSObject, NSCopying, NSSecureCoding {
  var skinType: HKFitzpatrickSkinType { get }
}
