
@available(iOS 8.0, *)
class CKModifySubscriptionsOperation : CKDatabaseOperation {
  init(subscriptionsToSave subscriptionsToSave: [CKSubscription]?, subscriptionIDsToDelete subscriptionIDsToDelete: [String]?)
  var subscriptionsToSave: [CKSubscription]?
  var subscriptionIDsToDelete: [String]?
  var modifySubscriptionsCompletionBlock: (([CKSubscription]?, [String]?, NSError?) -> Void)?
}
