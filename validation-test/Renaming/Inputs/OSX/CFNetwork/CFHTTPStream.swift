
@available(OSX 10.1, *)
let kCFStreamErrorDomainHTTP: Int32
enum CFStreamErrorHTTP : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case parseFailure
  case redirectionLoop
  case badURL
}
@available(OSX, introduced: 10.1, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPResponseHeader: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPFinalURL: CFString
@available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPFinalRequest: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPProxy: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPProxyHost: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPProxyPort: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPSProxyHost: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPSProxyPort: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPShouldAutoredirect: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPAttemptPersistentConnection: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSession API for http requests")
let kCFStreamPropertyHTTPRequestBytesWrittenCount: CFString
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
@discardableResult
func CFReadStreamCreateForHTTPRequest(_ alloc: CFAllocator?, _ request: CFHTTPMessage) -> Unmanaged<CFReadStream>
@available(OSX, introduced: 10.2, deprecated: 10.11, message: "Use NSURLSession API for http requests")
@discardableResult
func CFReadStreamCreateForStreamedHTTPRequest(_ alloc: CFAllocator?, _ requestHeaders: CFHTTPMessage, _ requestBody: CFReadStream) -> Unmanaged<CFReadStream>
