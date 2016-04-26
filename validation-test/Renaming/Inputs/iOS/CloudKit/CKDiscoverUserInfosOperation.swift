
@available(iOS 8.0, *)
class CKDiscoverUserInfosOperation : CKOperation {
  convenience init(emailAddresses emailAddresses: [String]?, userRecordIDs userRecordIDs: [CKRecordID]?)
  var emailAddresses: [String]?
  var userRecordIDs: [CKRecordID]?
  var discoverUserInfosCompletionBlock: (([String : CKDiscoveredUserInfo]?, [CKRecordID : CKDiscoveredUserInfo]?, NSError?) -> Void)?
}
