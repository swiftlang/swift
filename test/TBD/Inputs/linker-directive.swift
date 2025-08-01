@available(OSX 10.8, iOS 10.2, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 10.15, iOS 13)
public func toast() {}

@available(OSX 10.8, iOS 10.2, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 10.15, iOS 13)
public struct Vehicle {
  public func move() {}
  @available(macOS 10.15, iOS 13, *)
  public func originallyDefinedInCurrentModule() {}
}

@available(macOS 10.8, iOS 10.2, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 15, iOS 18)
public func movedPriorTo26() {}

@available(macOS 10.8, iOS 10.2, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 16, iOS 19)
public func movedInVersionsMappingTo26() {}

@available(macOS 10.8, iOS 10.2, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 17, iOS 20)
public func movedInVersionsMappingTo27() {}

@available(macOS 10.8, iOS 10.2, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 26, iOS 26)
public func movedInVersion26() {}
