@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
public func printMessageMoved() {
  print("Hello from LowLevel")
}

@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
public struct Entity {
    public let value = "LowLevel"
    public init() {}
    public func location() -> String { return "Entity from " + value }
}

// =================== Move protocol =================================//
@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
public protocol Box {
  associatedtype Item
  var ItemKind: String { get }
  func getItem() -> Item
  func shape() -> String
}

@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
extension Box {
  public func shape() -> String { return "round"}
}

@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
public struct Candy {
  public var kind = "candy"
  public init() {}
}

// =================== Move enum ============================ //
@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
public enum LanguageKind: Int {
  case Cpp = 1
  case Swift = 2
  case ObjC = 3
}

// =================== Move class ============================ //
@_originallyDefinedIn(module: "HighLevel", OSX 10.10)
open class Vehicle {
    public init() {}
    public var currentSpeed = 40.0
}
