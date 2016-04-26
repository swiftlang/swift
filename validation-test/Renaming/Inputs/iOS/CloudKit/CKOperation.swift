
@available(iOS 8.0, *)
class CKOperation : NSOperation {
  var container: CKContainer?
  @available(iOS, introduced: 8.0, deprecated: 9.0, message: "Set qualityOfService to NSQualityOfServiceUtility or NSQualityOfServiceBackground")
  var usesBackgroundSession: Bool
  var allowsCellularAccess: Bool
  @available(iOS 9.3, *)
  var operationID: String { get }
  @available(iOS 9.3, *)
  var isLongLived: Bool
  @available(iOS 9.3, *)
  var longLivedOperationWasPersistedBlock: () -> Void
}
