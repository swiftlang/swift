
class NSHost : NSObject {
  @discardableResult
  class func current() -> Self
  convenience init(name name: String?)
  convenience init(address address: String)
  @discardableResult
  func isEqual(to aHost: NSHost) -> Bool
  var name: String? { get }
  var names: [String] { get }
  var address: String? { get }
  var addresses: [String] { get }
  @available(OSX 10.6, *)
  var localizedName: String? { get }
}
