
@available(OSX 10.5, *)
class CBIdentityAuthority : NSObject {
  @discardableResult
  class func local() -> CBIdentityAuthority
  @discardableResult
  class func managed() -> CBIdentityAuthority
  @discardableResult
  class func defaultIdentityAuthority() -> CBIdentityAuthority
  var localizedName: String { get }
}
