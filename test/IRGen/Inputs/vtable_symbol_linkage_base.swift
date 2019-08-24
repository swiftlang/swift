
open class Base {
  private func privateFunc() -> Int {
    return 27
  }
  internal func internalFunc() -> Int {
    return 28
  }

  private var privateVar: Int = 29
  internal var internalVar: Int = 30
}

open class Middle : Base {
  public init(x: Int) {}
}

public struct Namespace {
  open class Nested {
    public init() {}
    var storedProp: Int?
  }
}

extension Namespace {
  open class ExtNested {
    public init() {}
    var storedProp: Int?
  }
}
