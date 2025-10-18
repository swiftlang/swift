// RUN: %target-swift-frontend -emit-ir %s

public protocol P1 {
  associatedtype Value
}

public protocol P2 {
  associatedtype Value
}

public class Class2<V> : P2 {
  public typealias Value = V
}

public class Superclass<T1, T2> where T1 : P1, T2 : P2, T2.Value == T1.Value {
}

public class Subclass<T1, T2> : Superclass<T1, T2> where T1 : P1, T2 : Class2<T1.Value> {
}
