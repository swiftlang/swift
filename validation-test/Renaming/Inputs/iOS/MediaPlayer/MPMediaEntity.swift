
typealias MPMediaEntityPersistentID = UInt64
@available(iOS 4.2, *)
class MPMediaEntity : NSObject, NSSecureCoding {
  @discardableResult
  class func canFilter(byProperty property: String) -> Bool
  @available(iOS 4.0, *)
  func enumerateValues(forProperties properties: Set<String>, using block: (String, AnyObject, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(iOS 8.0, *)
  subscript(_ key: AnyObject) -> AnyObject? { get }
  @discardableResult
  func value(forProperty property: String) -> AnyObject?
  @available(iOS 7.0, *)
  var persistentID: MPMediaEntityPersistentID { get }
}
@available(iOS 4.2, *)
let MPMediaEntityPropertyPersistentID: String
