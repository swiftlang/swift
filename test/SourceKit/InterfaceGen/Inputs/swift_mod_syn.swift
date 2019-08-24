
public protocol P1{
  associatedtype T1
  associatedtype T2
  func f1(t : T1) -> T1
  func f2(t : T2) -> T2
}

public protocol P2 {
	associatedtype P2T1
}

public extension P2 where P2T1 : P2{
	public func p2member() {}
}

public protocol P3 {}

public extension P1 where T1 : P2 {
  public func ef1(t : T1) {}
  public func ef2(t : T2) {}
}

public extension P1 where T1 == P3, T2 : P3 {
  public func ef3(t : T1) {}
  public func ef4(t : T1) {}
}

public extension P1 where T2 : P3 {
	public func ef5(t : T2) {}
}

public struct S2 {}

public struct S1<T> : P1, P2 {
  public typealias T1 = T
  public typealias T2 = S2
  public typealias P2T1 = T
  public func f1(t : T1) -> T1 {
    return t
  }
  public func f2(t : T2) -> T2 {
    return t
  }
}
