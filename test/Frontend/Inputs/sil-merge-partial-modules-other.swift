@inline(__always)
func internalFunction() {}

@_versioned
func versionedFunction() {}

public protocol Shape {
  func draw()
  var area: Float { get }
}

public class ShapeManager {
  public func manage() {}
}
