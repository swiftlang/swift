public struct Outer {
  public protocol Inner {
    func foo()
  }
}

extension Outer {
  public protocol InnerInExtension {
    func bar()
  }
}