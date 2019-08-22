public protocol P: C {}

public class C: P {
  public init() {}
  public func funcInClass() {}
}

public protocol GenericP: GenericC<Int> {}

public class GenericC<T> {
  public init() {}
  public func funcInClass() {}
}

extension GenericC: GenericP where T == Int {}
