
@available(OSX 10.11, *)
class WKSecurityOrigin : NSObject {
  var `protocol`: String { get }
  var host: String { get }
  var port: Int { get }
}
