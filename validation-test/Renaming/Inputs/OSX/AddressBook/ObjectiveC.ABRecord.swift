
class ABRecord : NSObject {
  @available(OSX 10.5, *)
  init!(addressBook addressBook: ABAddressBook!)
  @discardableResult
  func value(forProperty property: String!) -> AnyObject!
  @available(OSX 10.7, *)
  func setValue(_ value: AnyObject!, forProperty property: String!, error error: ()) throws
  @discardableResult
  func setValue(_ value: AnyObject!, forProperty property: String!) -> Bool
  @discardableResult
  func removeValue(forProperty property: String!) -> Bool
  @available(OSX 10.4, *)
  @discardableResult
  func isReadOnly() -> Bool
}
extension ABRecord {
  var uniqueId: String! { get }
  @available(OSX 10.10, *)
  var displayName: String! { get }
}
