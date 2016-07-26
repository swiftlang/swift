
var EAGL_MAJOR_VERSION: Int32 { get }
var EAGL_MINOR_VERSION: Int32 { get }
enum EAGLRenderingAPI : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case openGLES1
  case openGLES2
  case openGLES3
}
func EAGLGetVersion(_ major: UnsafeMutablePointer<UInt32>!, _ minor: UnsafeMutablePointer<UInt32>!)
class EAGLSharegroup : NSObject {
  @available(tvOS 6.0, *)
  var debugLabel: String!
}
class EAGLContext : NSObject {
  convenience init!(api api: EAGLRenderingAPI)
  init!(api api: EAGLRenderingAPI, sharegroup sharegroup: EAGLSharegroup!)
  @discardableResult
  class func setCurrentContext(_ context: EAGLContext!) -> Bool
  @discardableResult
  class func current() -> EAGLContext!
  var api: EAGLRenderingAPI { get }
  var sharegroup: EAGLSharegroup! { get }
  @available(tvOS 6.0, *)
  var debugLabel: String!
  @available(tvOS 7.1, *)
  var isMultiThreaded: Bool
}
