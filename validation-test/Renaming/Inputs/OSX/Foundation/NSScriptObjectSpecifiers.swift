
var NSNoSpecifierError: Int { get }
var NSNoTopLevelContainersSpecifierError: Int { get }
var NSContainerSpecifierError: Int { get }
var NSUnknownKeySpecifierError: Int { get }
var NSInvalidIndexSpecifierError: Int { get }
var NSInternalSpecifierError: Int { get }
var NSOperationNotSupportedForKeySpecifierError: Int { get }
enum NSInsertionPosition : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case positionAfter
  case positionBefore
  case positionBeginning
  case positionEnd
  case positionReplace
}
enum NSRelativePosition : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case after
  case before
}
enum NSWhoseSubelementIdentifier : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case indexSubelement
  case everySubelement
  case middleSubelement
  case randomSubelement
  case noSubelement
}
class NSScriptObjectSpecifier : NSObject, NSCoding {
  @available(OSX 10.5, *)
  /*not inherited*/ init?(descriptor descriptor: NSAppleEventDescriptor)
  convenience init(containerSpecifier container: NSScriptObjectSpecifier, key property: String)
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String)
  unowned(unsafe) var child: @sil_unmanaged NSScriptObjectSpecifier?
  var container: NSScriptObjectSpecifier?
  var containerIsObjectBeingTested: Bool
  var containerIsRangeContainerObject: Bool
  var key: String
  var containerClassDescription: NSScriptClassDescription?
  var keyClassDescription: NSScriptClassDescription? { get }
  @discardableResult
  func indicesOfObjectsByEvaluating(withContainer container: AnyObject, count count: UnsafeMutablePointer<Int>) -> UnsafeMutablePointer<Int>?
  @discardableResult
  func objectsByEvaluating(withContainers containers: AnyObject) -> AnyObject?
  var objectsByEvaluatingSpecifier: AnyObject? { get }
  var evaluationErrorNumber: Int
  var evaluationError: NSScriptObjectSpecifier? { get }
  @available(OSX 10.5, *)
  @NSCopying var descriptor: NSAppleEventDescriptor? { get }
}
extension NSObject {
  var objectSpecifier: NSScriptObjectSpecifier? { get }
  @discardableResult
  class func indicesOfObjects(byEvaluatingObjectSpecifier specifier: NSScriptObjectSpecifier) -> [NSNumber]?
  @discardableResult
  func indicesOfObjects(byEvaluatingObjectSpecifier specifier: NSScriptObjectSpecifier) -> [NSNumber]?
  class func objectSpecifier() -> NSScriptObjectSpecifier?
}
class NSIndexSpecifier : NSScriptObjectSpecifier {
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String, index index: Int)
  var index: Int
}
class NSMiddleSpecifier : NSScriptObjectSpecifier {
}
class NSNameSpecifier : NSScriptObjectSpecifier {
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String, name name: String)
  var name: String
}
class NSPositionalSpecifier : NSObject {
  init(position position: NSInsertionPosition, objectSpecifier specifier: NSScriptObjectSpecifier)
  @available(OSX 10.5, *)
  var position: NSInsertionPosition { get }
  func setInsertionClassDescription(_ classDescription: NSScriptClassDescription)
  func evaluate()
  var insertionContainer: AnyObject? { get }
  var insertionKey: String? { get }
  var insertionIndex: Int { get }
  var insertionReplaces: Bool { get }
}
class NSPropertySpecifier : NSScriptObjectSpecifier {
}
class NSRandomSpecifier : NSScriptObjectSpecifier {
}
class NSRangeSpecifier : NSScriptObjectSpecifier {
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String, start startSpec: NSScriptObjectSpecifier?, end endSpec: NSScriptObjectSpecifier?)
  var startSpecifier: NSScriptObjectSpecifier?
  var endSpecifier: NSScriptObjectSpecifier?
}
class NSRelativeSpecifier : NSScriptObjectSpecifier {
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String, relativePosition relPos: NSRelativePosition, baseSpecifier baseSpecifier: NSScriptObjectSpecifier?)
  var relativePosition: NSRelativePosition
  var baseSpecifier: NSScriptObjectSpecifier?
}
class NSUniqueIDSpecifier : NSScriptObjectSpecifier {
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String, uniqueID uniqueID: AnyObject)
  @NSCopying var uniqueID: AnyObject
}
class NSWhoseSpecifier : NSScriptObjectSpecifier {
  init(containerClassDescription classDesc: NSScriptClassDescription, containerSpecifier container: NSScriptObjectSpecifier?, key property: String, test test: NSScriptWhoseTest)
  var test: NSScriptWhoseTest
  var startSubelementIdentifier: NSWhoseSubelementIdentifier
  var startSubelementIndex: Int
  var endSubelementIdentifier: NSWhoseSubelementIdentifier
  var endSubelementIndex: Int
}
