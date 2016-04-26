
@available(OSX 10.10, *)
class CKDiscoverAllContactsOperation : CKOperation {
  var discoverAllContactsCompletionBlock: (([CKDiscoveredUserInfo]?, NSError?) -> Void)?
}
