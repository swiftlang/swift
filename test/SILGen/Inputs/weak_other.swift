public struct Flags {
  public func asBoolean() -> Bool { return true }
}

public protocol Router : class {

}

extension Router {
  public var flags: Flags { return Flags() }
}

public protocol Environment : class {
  unowned var router: Router { get }
}

open class UI {
  open unowned let environment: Environment

  init(e: Environment) {
    environment = e
  }
}
