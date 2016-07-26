
enum NSSaveOptions : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case yes
  case no
  case ask
}
class NSCloneCommand : NSScriptCommand {
  var keySpecifier: NSScriptObjectSpecifier { get }
}
class NSCloseCommand : NSScriptCommand {
  var saveOptions: NSSaveOptions { get }
}
class NSCountCommand : NSScriptCommand {
}
class NSCreateCommand : NSScriptCommand {
  var createClassDescription: NSScriptClassDescription { get }
  var resolvedKeyDictionary: [String : AnyObject] { get }
}
class NSDeleteCommand : NSScriptCommand {
  var keySpecifier: NSScriptObjectSpecifier { get }
}
class NSExistsCommand : NSScriptCommand {
}
class NSGetCommand : NSScriptCommand {
}
class NSMoveCommand : NSScriptCommand {
  var keySpecifier: NSScriptObjectSpecifier { get }
}
class NSQuitCommand : NSScriptCommand {
  var saveOptions: NSSaveOptions { get }
}
class NSSetCommand : NSScriptCommand {
  var keySpecifier: NSScriptObjectSpecifier { get }
}
