@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
public func printMessageMoved() {
  print("Hello from LowLevel")
}

@available(OSX 10.7, iOS 7.0, *)
@available(unrecognizedOS 1.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
@_originallyDefinedIn(module: "HighLevel", unrecognizedOS 2.0)
public struct Entity {
    public let value = "LowLevel"
    public init() {}
    public func location() -> String { return "Entity from " + value }
}

// =================== Move protocol =================================//
@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
public protocol Box {
  associatedtype Item
  var ItemKind: String { get }
  func getItem() -> Item
  func shape() -> String
}

@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
extension Box {
  public func shape() -> String { return "round"}
}

@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
public struct Candy {
  public var kind = "candy"
  public init() {}
}

// =================== Move enum ============================ //
@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
public enum LanguageKind: Int {
  case Cpp = 1
  case Swift = 2
  case ObjC = 3
}

// =================== Move class ============================ //
@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0)
open class Vehicle {
    public init() {}
    public var currentSpeed = 40.0
}

// =================== Move from macro ======================= //
@available(OSX 10.7, iOS 7.0, *)
@_originallyDefinedIn(module: "HighLevel", _iOS13Aligned)
public func funcMacro () { print("Macro from LowLevel") }
