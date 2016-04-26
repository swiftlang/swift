
@available(OSX 10.10, *)
class CKMarkNotificationsReadOperation : CKOperation {
  init(notificationIDsToMarkRead notificationIDs: [CKNotificationID])
  var notificationIDs: [CKNotificationID]
  var markNotificationsReadCompletionBlock: (([CKNotificationID]?, NSError?) -> Void)?
}
