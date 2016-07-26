
@available(OSX 10.10, *)
let WKErrorDomain: String
@available(OSX 10.10, *)
enum WKErrorCode : Int {
  case unknown
  case webContentProcessTerminated
  case webViewInvalidated
  case javaScriptExceptionOccurred
  @available(OSX 10.11, *)
  case javaScriptResultTypeIsUnsupported
}

@available(OSX 10.10, iOS 8.0, *)
extension WKErrorCode : _BridgedNSError {
}
