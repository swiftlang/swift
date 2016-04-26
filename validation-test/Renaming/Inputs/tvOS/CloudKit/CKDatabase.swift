
@available(tvOS 8.0, *)
class CKDatabase : NSObject {
  func add(_ operation: CKDatabaseOperation)
}
extension CKDatabase {
  func fetchRecord(with recordID: CKRecordID, completionHandler completionHandler: (CKRecord?, NSError?) -> Void)
  func save(_ record: CKRecord, completionHandler completionHandler: (CKRecord?, NSError?) -> Void)
  func deleteRecord(with recordID: CKRecordID, completionHandler completionHandler: (CKRecordID?, NSError?) -> Void)
  func perform(_ query: CKQuery, inZoneWith zoneID: CKRecordZoneID?, completionHandler completionHandler: ([CKRecord]?, NSError?) -> Void)
  func fetchAllRecordZones(completionHandler completionHandler: ([CKRecordZone]?, NSError?) -> Void)
  func fetchRecordZone(with zoneID: CKRecordZoneID, completionHandler completionHandler: (CKRecordZone?, NSError?) -> Void)
  func save(_ zone: CKRecordZone, completionHandler completionHandler: (CKRecordZone?, NSError?) -> Void)
  func deleteRecordZone(with zoneID: CKRecordZoneID, completionHandler completionHandler: (CKRecordZoneID?, NSError?) -> Void)
  func fetchSubscription(withID subscriptionID: String, completionHandler completionHandler: (CKSubscription?, NSError?) -> Void)
  func fetchAllSubscriptions(completionHandler completionHandler: ([CKSubscription]?, NSError?) -> Void)
  func save(_ subscription: CKSubscription, completionHandler completionHandler: (CKSubscription?, NSError?) -> Void)
  func deleteSubscription(withID subscriptionID: String, completionHandler completionHandler: (String?, NSError?) -> Void)
}
