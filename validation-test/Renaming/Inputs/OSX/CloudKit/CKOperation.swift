
@available(OSX 10.10, *)
class CKOperation : NSOperation {
  var container: CKContainer?
  @available(OSX, introduced: 10.10, deprecated: 10.11, message: "Set qualityOfService to NSQualityOfServiceUtility or NSQualityOfServiceBackground")
  var usesBackgroundSession: Bool
  var allowsCellularAccess: Bool
  @available(OSX 10.12, *)
  var operationID: String { get }
  @available(OSX 10.12, *)
  var isLongLived: Bool
  @available(OSX 10.12, *)
  var longLivedOperationWasPersistedBlock: () -> Void
}
