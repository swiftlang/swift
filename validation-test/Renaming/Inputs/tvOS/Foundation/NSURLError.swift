
let NSURLErrorDomain: String
@available(tvOS 4.0, *)
let NSURLErrorFailingURLErrorKey: String
@available(tvOS 4.0, *)
let NSURLErrorFailingURLStringErrorKey: String
@available(tvOS 4.0, *)
let NSURLErrorFailingURLPeerTrustErrorKey: String
@available(tvOS 8.0, *)
let NSURLErrorBackgroundTaskCancelledReasonKey: String
var NSURLErrorCancelledReasonUserForceQuitApplication: Int { get }
var NSURLErrorCancelledReasonBackgroundUpdatesDisabled: Int { get }
@available(tvOS 8.0, *)
var NSURLErrorCancelledReasonInsufficientSystemResources: Int { get }
var NSURLErrorUnknown: Int { get }
var NSURLErrorCancelled: Int { get }
var NSURLErrorBadURL: Int { get }
var NSURLErrorTimedOut: Int { get }
var NSURLErrorUnsupportedURL: Int { get }
var NSURLErrorCannotFindHost: Int { get }
var NSURLErrorCannotConnectToHost: Int { get }
var NSURLErrorNetworkConnectionLost: Int { get }
var NSURLErrorDNSLookupFailed: Int { get }
var NSURLErrorHTTPTooManyRedirects: Int { get }
var NSURLErrorResourceUnavailable: Int { get }
var NSURLErrorNotConnectedToInternet: Int { get }
var NSURLErrorRedirectToNonExistentLocation: Int { get }
var NSURLErrorBadServerResponse: Int { get }
var NSURLErrorUserCancelledAuthentication: Int { get }
var NSURLErrorUserAuthenticationRequired: Int { get }
var NSURLErrorZeroByteResource: Int { get }
var NSURLErrorCannotDecodeRawData: Int { get }
var NSURLErrorCannotDecodeContentData: Int { get }
var NSURLErrorCannotParseResponse: Int { get }
@available(tvOS 9.0, *)
var NSURLErrorAppTransportSecurityRequiresSecureConnection: Int { get }
var NSURLErrorFileDoesNotExist: Int { get }
var NSURLErrorFileIsDirectory: Int { get }
var NSURLErrorNoPermissionsToReadFile: Int { get }
@available(tvOS 2.0, *)
var NSURLErrorDataLengthExceedsMaximum: Int { get }
var NSURLErrorSecureConnectionFailed: Int { get }
var NSURLErrorServerCertificateHasBadDate: Int { get }
var NSURLErrorServerCertificateUntrusted: Int { get }
var NSURLErrorServerCertificateHasUnknownRoot: Int { get }
var NSURLErrorServerCertificateNotYetValid: Int { get }
var NSURLErrorClientCertificateRejected: Int { get }
var NSURLErrorClientCertificateRequired: Int { get }
var NSURLErrorCannotLoadFromNetwork: Int { get }
var NSURLErrorCannotCreateFile: Int { get }
var NSURLErrorCannotOpenFile: Int { get }
var NSURLErrorCannotCloseFile: Int { get }
var NSURLErrorCannotWriteToFile: Int { get }
var NSURLErrorCannotRemoveFile: Int { get }
var NSURLErrorCannotMoveFile: Int { get }
var NSURLErrorDownloadDecodingFailedMidStream: Int { get }
var NSURLErrorDownloadDecodingFailedToComplete: Int { get }
@available(tvOS 3.0, *)
var NSURLErrorInternationalRoamingOff: Int { get }
@available(tvOS 3.0, *)
var NSURLErrorCallIsActive: Int { get }
@available(tvOS 3.0, *)
var NSURLErrorDataNotAllowed: Int { get }
@available(tvOS 3.0, *)
var NSURLErrorRequestBodyStreamExhausted: Int { get }
@available(tvOS 8.0, *)
var NSURLErrorBackgroundSessionRequiresSharedContainer: Int { get }
@available(tvOS 8.0, *)
var NSURLErrorBackgroundSessionInUseByAnotherProcess: Int { get }
@available(tvOS 8.0, *)
var NSURLErrorBackgroundSessionWasDisconnected: Int { get }
