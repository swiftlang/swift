public func printMessageMoved() {
  print("Hello from HighLevel")
}
public func printMessage() {
  printMessageMoved()
}

public struct Entity {
    public let value = "HighLevel"
    public init() {}
    public func location() -> String { return "Entity from " + value }
}

// =================== Move protocol ================================= //
public protocol Box {
  associatedtype Item
  var ItemKind: String { get }
  func getItem() -> Item
  func shape() -> String
}

extension Box {
  public func shape() -> String { return "square"}
}

public struct Candy {
  public var kind = "candy"
  public init() {}
}

public class CandyBox: Box {
  public typealias Item = Candy
  public var ItemKind: String { return getItem().kind }
  let itemInBox: Item
  public init(_ itemInBox: Item) { self.itemInBox = itemInBox }
  public func getItem() -> Item { return itemInBox }
}

// =================== Move enum ============================ //
public enum LanguageKind: Int {
  case Cpp = -1
  case Swift = -2
  case ObjC = -3
}

open class Vehicle {
    public init() {}
    public var currentSpeed = -40.0
}
