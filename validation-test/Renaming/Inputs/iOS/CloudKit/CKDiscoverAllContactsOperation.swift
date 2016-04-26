
@available(iOS 8.0, *)
class CKDiscoverAllContactsOperation : CKOperation {
  var discoverAllContactsCompletionBlock: (([CKDiscoveredUserInfo]?, NSError?) -> Void)?
}
