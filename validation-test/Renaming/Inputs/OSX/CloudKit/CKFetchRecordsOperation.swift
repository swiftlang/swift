
@available(OSX 10.10, *)
class CKFetchRecordsOperation : CKDatabaseOperation {
  convenience init(recordIDs recordIDs: [CKRecordID])
  @discardableResult
  class func fetchCurrentUserRecord() -> Self
  var recordIDs: [CKRecordID]?
  var desiredKeys: [String]?
  var perRecordProgressBlock: ((CKRecordID, Double) -> Void)?
  var perRecordCompletionBlock: ((CKRecord?, CKRecordID?, NSError?) -> Void)?
  var fetchRecordsCompletionBlock: (([CKRecordID : CKRecord]?, NSError?) -> Void)?
}
