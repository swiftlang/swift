
@available(watchOS 2.0, *)
class NSUUID : NSObject, NSCopying, NSSecureCoding {
  convenience init?(uuidString string: String)
  convenience init(uuidBytes bytes: UnsafePointer<UInt8>!)
  func getBytes(_ uuid: UnsafeMutablePointer<UInt8>!)
  var uuidString: String { get }
}
