
@available(iOS 8.0, *)
class CKQueryCursor : NSObject, NSCopying, NSSecureCoding {
}
@available(iOS 8.0, *)
let CKQueryOperationMaximumResults: Int
@available(iOS 8.0, *)
class CKQueryOperation : CKDatabaseOperation {
  convenience init(query query: CKQuery)
  convenience init(cursor cursor: CKQueryCursor)
  @NSCopying var query: CKQuery?
  @NSCopying var cursor: CKQueryCursor?
  @NSCopying var zoneID: CKRecordZoneID?
  var resultsLimit: Int
  var desiredKeys: [String]?
  var recordFetchedBlock: ((CKRecord) -> Void)?
  var queryCompletionBlock: ((CKQueryCursor?, NSError?) -> Void)?
}
