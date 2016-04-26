
class NSScriptSuiteRegistry : NSObject {
  @discardableResult
  class func shared() -> NSScriptSuiteRegistry
  class func setSharedScriptSuiteRegistry(_ registry: NSScriptSuiteRegistry)
  func loadSuites(from bundle: NSBundle)
  func loadSuite(with suiteDeclaration: [NSObject : AnyObject], from bundle: NSBundle)
  func register(_ classDescription: NSScriptClassDescription)
  func register(_ commandDescription: NSScriptCommandDescription)
  var suiteNames: [String] { get }
  @discardableResult
  func appleEventCode(forSuite suiteName: String) -> FourCharCode
  @discardableResult
  func bundle(forSuite suiteName: String) -> NSBundle?
  @discardableResult
  func classDescriptions(inSuite suiteName: String) -> [String : NSScriptClassDescription]?
  @discardableResult
  func commandDescriptions(inSuite suiteName: String) -> [String : NSScriptCommandDescription]?
  @discardableResult
  func suite(forAppleEventCode appleEventCode: FourCharCode) -> String?
  @discardableResult
  func classDescription(withAppleEventCode appleEventCode: FourCharCode) -> NSScriptClassDescription?
  @discardableResult
  func commandDescription(withAppleEventClass appleEventClassCode: FourCharCode, andAppleEventCode appleEventIDCode: FourCharCode) -> NSScriptCommandDescription?
  @discardableResult
  func aeteResource(_ languageName: String) -> NSData?
}
