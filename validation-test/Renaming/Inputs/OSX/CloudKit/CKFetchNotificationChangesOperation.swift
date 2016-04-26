
@available(OSX 10.10, *)
class CKFetchNotificationChangesOperation : CKOperation {
  init(previousServerChangeToken previousServerChangeToken: CKServerChangeToken?)
  @NSCopying var previousServerChangeToken: CKServerChangeToken?
  var resultsLimit: Int
  var moreComing: Bool { get }
  var notificationChangedBlock: ((CKNotification) -> Void)?
  var fetchNotificationChangesCompletionBlock: ((CKServerChangeToken?, NSError?) -> Void)?
}
