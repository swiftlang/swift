
class NSScriptClassDescription : NSClassDescription {
  init?(suiteName suiteName: String, className className: String, dictionary classDeclaration: [NSObject : AnyObject]?)
  var suiteName: String? { get }
  var implementationClassName: String? { get }
  var superclass: NSScriptClassDescription? { get }
  var appleEventCode: FourCharCode { get }
  @discardableResult
  func matchesAppleEventCode(_ appleEventCode: FourCharCode) -> Bool
  @discardableResult
  func supportsCommand(_ commandDescription: NSScriptCommandDescription) -> Bool
  @discardableResult
  func selector(forCommand commandDescription: NSScriptCommandDescription) -> Selector?
  @discardableResult
  func type(forKey key: String) -> String?
  @discardableResult
  func forKey(_ key: String) -> NSScriptClassDescription?
  @discardableResult
  func appleEventCode(forKey key: String) -> FourCharCode
  @discardableResult
  func key(withAppleEventCode appleEventCode: FourCharCode) -> String?
  var defaultSubcontainerAttributeKey: String? { get }
  @discardableResult
  func isLocationRequiredToCreate(forKey toManyRelationshipKey: String) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func hasProperty(forKey key: String) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func hasOrderedToManyRelationship(forKey key: String) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func hasReadableProperty(forKey key: String) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func hasWritableProperty(forKey key: String) -> Bool
}
extension NSScriptClassDescription {
}
extension NSObject {
  var classCode: FourCharCode { get }
  var className: String { get }
  class func classCode() -> FourCharCode
  class func className() -> String
}
