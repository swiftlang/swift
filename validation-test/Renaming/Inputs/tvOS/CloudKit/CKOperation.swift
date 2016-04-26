
@available(tvOS 8.0, *)
class CKOperation : NSOperation {
  var container: CKContainer?
  @available(tvOS, introduced: 8.0, deprecated: 9.0, message: "Set qualityOfService to NSQualityOfServiceUtility or NSQualityOfServiceBackground")
  var usesBackgroundSession: Bool
  var allowsCellularAccess: Bool
  @available(tvOS 9.3, *)
  var operationID: String { get }
  @available(tvOS 9.3, *)
  var isLongLived: Bool
  @available(tvOS 9.3, *)
  var longLivedOperationWasPersistedBlock: () -> Void
}
