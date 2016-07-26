
@available(iOS 8.0, *)
let CKErrorDomain: String
@available(iOS 8.0, *)
let CKPartialErrorsByItemIDKey: String
@available(iOS 8.0, *)
let CKRecordChangedErrorAncestorRecordKey: String
@available(iOS 8.0, *)
let CKRecordChangedErrorServerRecordKey: String
@available(iOS 8.0, *)
let CKRecordChangedErrorClientRecordKey: String
@available(iOS 8.0, *)
let CKErrorRetryAfterKey: String
@available(iOS 8.0, *)
enum CKErrorCode : Int {
  case internalError
  case partialFailure
  case networkUnavailable
  case networkFailure
  case badContainer
  case serviceUnavailable
  case requestRateLimited
  case missingEntitlement
  case notAuthenticated
  case permissionFailure
  case unknownItem
  case invalidArguments
  case resultsTruncated
  case serverRecordChanged
  case serverRejectedRequest
  case assetFileNotFound
  case assetFileModified
  case incompatibleVersion
  case constraintViolation
  case operationCancelled
  case changeTokenExpired
  case batchRequestFailed
  case zoneBusy
  case badDatabase
  case quotaExceeded
  case zoneNotFound
  case limitExceeded
  case userDeletedZone
}

@available(OSX 10.10, iOS 8.0, *)
extension CKErrorCode : _BridgedNSError {
}
