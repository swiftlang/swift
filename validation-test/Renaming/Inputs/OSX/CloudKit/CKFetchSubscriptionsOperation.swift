
@available(OSX 10.10, *)
class CKFetchSubscriptionsOperation : CKDatabaseOperation {
  @discardableResult
  class func fetchAll() -> Self
  convenience init(subscriptionIDs subscriptionIDs: [String])
  var subscriptionIDs: [String]?
  var fetchSubscriptionCompletionBlock: (([String : CKSubscription]?, NSError?) -> Void)?
}
