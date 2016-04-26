
class NSScriptCommandDescription : NSObject, NSCoding {
  init?(suiteName suiteName: String, commandName commandName: String, dictionary commandDeclaration: [NSObject : AnyObject]?)
  var suiteName: String { get }
  var commandName: String { get }
  var appleEventClassCode: FourCharCode { get }
  var appleEventCode: FourCharCode { get }
  var commandClassName: String { get }
  var returnType: String? { get }
  var appleEventCodeForReturnType: FourCharCode { get }
  var argumentNames: [String] { get }
  @discardableResult
  func typeForArgument(withName argumentName: String) -> String?
  @discardableResult
  func appleEventCodeForArgument(withName argumentName: String) -> FourCharCode
  @discardableResult
  func isOptionalArgument(withName argumentName: String) -> Bool
  @discardableResult
  func createCommandInstance() -> NSScriptCommand
  @discardableResult
  func createCommandInstance(with zone: NSZone? = nil) -> NSScriptCommand
}
