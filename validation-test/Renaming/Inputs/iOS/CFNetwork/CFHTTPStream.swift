
@available(iOS 2.0, *)
let kCFStreamErrorDomainHTTP: Int32
enum CFStreamErrorHTTP : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case parseFailure
  case redirectionLoop
  case badURL
}
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPResponseHeader: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPFinalURL: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPFinalRequest: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPProxy: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPProxyHost: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPProxyPort: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPSProxyHost: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPSProxyPort: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPShouldAutoredirect: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPAttemptPersistentConnection: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPRequestBytesWrittenCount: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
@discardableResult
func CFReadStreamCreateForHTTPRequest(_ alloc: CFAllocator?, _ request: CFHTTPMessage) -> Unmanaged<CFReadStream>
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSession API for http requests")
@discardableResult
func CFReadStreamCreateForStreamedHTTPRequest(_ alloc: CFAllocator?, _ requestHeaders: CFHTTPMessage, _ requestBody: CFReadStream) -> Unmanaged<CFReadStream>
