
@available(iOS 8.0, *)
class CKFetchRecordChangesOperation : CKDatabaseOperation {
  init(recordZoneID recordZoneID: CKRecordZoneID, previousServerChangeToken previousServerChangeToken: CKServerChangeToken?)
  @NSCopying var recordZoneID: CKRecordZoneID
  @NSCopying var previousServerChangeToken: CKServerChangeToken?
  var resultsLimit: Int
  var desiredKeys: [String]?
  var recordChangedBlock: ((CKRecord) -> Void)?
  var recordWithIDWasDeletedBlock: ((CKRecordID) -> Void)?
  var moreComing: Bool { get }
  var fetchRecordChangesCompletionBlock: ((CKServerChangeToken?, NSData?, NSError?) -> Void)?
}
