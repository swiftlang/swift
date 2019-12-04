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
