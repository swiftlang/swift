public class C {}

public struct S {
  public let c: C

  public init() {
    self.c = C()
  }
}
