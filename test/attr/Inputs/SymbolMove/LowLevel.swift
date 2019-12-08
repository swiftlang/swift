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
