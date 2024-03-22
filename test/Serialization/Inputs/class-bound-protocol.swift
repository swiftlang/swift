//

public protocol Base {}

extension Base {
  public func funcInBaseProtocol() {}
}

//

public class C: Base {
  public init() {}
  public func funcInClass() {}
}

//

public protocol P1: C {}

public protocol P2 where Self: C {}

public class D: C, P1, P2 {}

//

public class GenericC<T>: Base {
  public init() {}
  public func funcInClass() {}
}

//

public protocol GenericP1: GenericC<Int> {}

public protocol GenericP2 where Self: GenericC<Int> {}

public class GenericD<T>: GenericC<T> {}

extension GenericD: GenericP1, GenericP2 where T == Int {}
