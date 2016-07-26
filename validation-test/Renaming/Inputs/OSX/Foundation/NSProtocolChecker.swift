
class NSProtocolChecker : NSProxy {
  var `protocol`: Protocol { get }
  var target: NSObject? { get }
}
extension NSProtocolChecker {
  init(target anObject: NSObject, protocol aProtocol: Protocol)
}
