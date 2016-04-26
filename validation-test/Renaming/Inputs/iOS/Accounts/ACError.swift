
@available(iOS 5.0, *)
let ACErrorDomain: String
struct ACErrorCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var ACErrorUnknown: ACErrorCode { get }
var ACErrorAccountMissingRequiredProperty: ACErrorCode { get }
var ACErrorAccountAuthenticationFailed: ACErrorCode { get }
var ACErrorAccountTypeInvalid: ACErrorCode { get }
var ACErrorAccountAlreadyExists: ACErrorCode { get }
var ACErrorAccountNotFound: ACErrorCode { get }
var ACErrorPermissionDenied: ACErrorCode { get }
var ACErrorAccessInfoInvalid: ACErrorCode { get }
var ACErrorClientPermissionDenied: ACErrorCode { get }
var ACErrorAccessDeniedByProtectionPolicy: ACErrorCode { get }
var ACErrorCredentialNotFound: ACErrorCode { get }
var ACErrorFetchCredentialFailed: ACErrorCode { get }
var ACErrorStoreCredentialFailed: ACErrorCode { get }
var ACErrorRemoveCredentialFailed: ACErrorCode { get }
var ACErrorUpdatingNonexistentAccount: ACErrorCode { get }
var ACErrorInvalidClientBundleID: ACErrorCode { get }
var ACErrorDeniedByPlugin: ACErrorCode { get }
var ACErrorCoreDataSaveFailed: ACErrorCode { get }
var ACErrorFailedSerializingAccountInfo: ACErrorCode { get }
var ACErrorInvalidCommand: ACErrorCode { get }
var ACErrorMissingTransportMessageID: ACErrorCode { get }
var ACErrorCredentialItemNotFound: ACErrorCode { get }
var ACErrorCredentialItemNotExpired: ACErrorCode { get }
