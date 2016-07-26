
@available(tvOS 2.0, *)
let kCFErrorDomainCFNetwork: CFString
@available(tvOS 2.0, *)
let kCFErrorDomainWinSock: CFString
enum CFNetworkErrors : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case cfHostErrorHostNotFound
  case cfHostErrorUnknown
  case cfsocksErrorUnknownClientVersion
  case cfsocksErrorUnsupportedServerVersion
  case cfsocks4ErrorRequestFailed
  case cfsocks4ErrorIdentdFailed
  case cfsocks4ErrorIdConflict
  case cfsocks4ErrorUnknownStatusCode
  case cfsocks5ErrorBadState
  case cfsocks5ErrorBadResponseAddr
  case cfsocks5ErrorBadCredentials
  case cfsocks5ErrorUnsupportedNegotiationMethod
  case cfsocks5ErrorNoAcceptableMethod
  case cfftpErrorUnexpectedStatusCode
  case cfErrorHTTPAuthenticationTypeUnsupported
  case cfErrorHTTPBadCredentials
  case cfErrorHTTPConnectionLost
  case cfErrorHTTPParseFailure
  case cfErrorHTTPRedirectionLoopDetected
  case cfErrorHTTPBadURL
  case cfErrorHTTPProxyConnectionFailure
  case cfErrorHTTPBadProxyCredentials
  case cfErrorPACFileError
  case cfErrorPACFileAuth
  case cfErrorHTTPSProxyConnectionFailure
  case cfStreamErrorHTTPSProxyFailureUnexpectedResponseToCONNECTMethod
  case cfurlErrorBackgroundSessionInUseByAnotherProcess
  case cfurlErrorBackgroundSessionWasDisconnected
  case cfurlErrorUnknown
  case cfurlErrorCancelled
  case cfurlErrorBadURL
  case cfurlErrorTimedOut
  case cfurlErrorUnsupportedURL
  case cfurlErrorCannotFindHost
  case cfurlErrorCannotConnectToHost
  case cfurlErrorNetworkConnectionLost
  case cfurlErrorDNSLookupFailed
  case cfurlErrorHTTPTooManyRedirects
  case cfurlErrorResourceUnavailable
  case cfurlErrorNotConnectedToInternet
  case cfurlErrorRedirectToNonExistentLocation
  case cfurlErrorBadServerResponse
  case cfurlErrorUserCancelledAuthentication
  case cfurlErrorUserAuthenticationRequired
  case cfurlErrorZeroByteResource
  case cfurlErrorCannotDecodeRawData
  case cfurlErrorCannotDecodeContentData
  case cfurlErrorCannotParseResponse
  case cfurlErrorInternationalRoamingOff
  case cfurlErrorCallIsActive
  case cfurlErrorDataNotAllowed
  case cfurlErrorRequestBodyStreamExhausted
  case cfurlErrorAppTransportSecurityRequiresSecureConnection
  case cfurlErrorFileDoesNotExist
  case cfurlErrorFileIsDirectory
  case cfurlErrorNoPermissionsToReadFile
  case cfurlErrorDataLengthExceedsMaximum
  case cfurlErrorSecureConnectionFailed
  case cfurlErrorServerCertificateHasBadDate
  case cfurlErrorServerCertificateUntrusted
  case cfurlErrorServerCertificateHasUnknownRoot
  case cfurlErrorServerCertificateNotYetValid
  case cfurlErrorClientCertificateRejected
  case cfurlErrorClientCertificateRequired
  case cfurlErrorCannotLoadFromNetwork
  case cfurlErrorCannotCreateFile
  case cfurlErrorCannotOpenFile
  case cfurlErrorCannotCloseFile
  case cfurlErrorCannotWriteToFile
  case cfurlErrorCannotRemoveFile
  case cfurlErrorCannotMoveFile
  case cfurlErrorDownloadDecodingFailedMidStream
  case cfurlErrorDownloadDecodingFailedToComplete
  case cfhttpCookieCannotParseCookieFile
  case cfNetServiceErrorUnknown
  case cfNetServiceErrorCollision
  case cfNetServiceErrorNotFound
  case cfNetServiceErrorInProgress
  case cfNetServiceErrorBadArgument
  case cfNetServiceErrorCancel
  case cfNetServiceErrorInvalid
  case cfNetServiceErrorTimeout
  case cfNetServiceErrorDNSServiceFailure
}
@available(tvOS 2.2, *)
let kCFURLErrorFailingURLErrorKey: CFString
@available(tvOS 2.2, *)
let kCFURLErrorFailingURLStringErrorKey: CFString
@available(tvOS 2.0, *)
let kCFGetAddrInfoFailureKey: CFString
@available(tvOS 2.0, *)
let kCFSOCKSStatusCodeKey: CFString
@available(tvOS 2.0, *)
let kCFSOCKSVersionKey: CFString
@available(tvOS 2.0, *)
let kCFSOCKSNegotiationMethodKey: CFString
@available(tvOS 2.0, *)
let kCFDNSServiceFailureKey: CFString
@available(tvOS 2.0, *)
let kCFFTPStatusCodeKey: CFString
