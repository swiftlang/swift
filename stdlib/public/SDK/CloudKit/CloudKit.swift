@_exported import CloudKit
import Foundation

@available(macOS 10.10, iOS 8.0, *)
extension CKError {
  /// Retrieve partial error results associated by item ID.
  public var partialErrorsByItemID: [NSObject : Error]? {
    return userInfo[CKPartialErrorsByItemIDKey] as? [NSObject : Error]
  }

  /// The original CKRecord object that you used as the basis for
  /// making your changes.
  public var ancestorRecord: CKRecord? {
    return userInfo[CKRecordChangedErrorAncestorRecordKey] as? CKRecord
  }

  /// The CKRecord object that was found on the server. Use this
  /// record as the basis for merging your changes.
  public var serverRecord: CKRecord? {
    return userInfo[CKRecordChangedErrorServerRecordKey] as? CKRecord
  }

  /// The CKRecord object that you tried to save. This record is based
  /// on the record in the CKRecordChangedErrorAncestorRecordKey key
  /// but contains the additional changes you made.
  public var clientRecord: CKRecord? {
    return userInfo[CKRecordChangedErrorClientRecordKey] as? CKRecord
  }

  /// The number of seconds after which you may retry a request. This
  /// key may be included in an error of type
  /// `CKErrorServiceUnavailable` or `CKErrorRequestRateLimited`.
  public var retryAfterSeconds: Double? {
    return userInfo[CKErrorRetryAfterKey] as? Double
  }
}
