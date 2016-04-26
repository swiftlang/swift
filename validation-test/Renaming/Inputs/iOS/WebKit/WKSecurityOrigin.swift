
@available(iOS 9.0, *)
class WKSecurityOrigin : NSObject {
  var `protocol`: String { get }
  var host: String { get }
  var port: Int { get }
}
