
@available(OSX 10.10, *)
class CKModifyRecordZonesOperation : CKDatabaseOperation {
  convenience init(recordZonesToSave recordZonesToSave: [CKRecordZone]?, recordZoneIDsToDelete recordZoneIDsToDelete: [CKRecordZoneID]?)
  var recordZonesToSave: [CKRecordZone]?
  var recordZoneIDsToDelete: [CKRecordZoneID]?
  var modifyRecordZonesCompletionBlock: (([CKRecordZone]?, [CKRecordZoneID]?, NSError?) -> Void)?
}
