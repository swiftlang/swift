public protocol P {}

extension Int: P {}
extension String: P {}
extension Bool: P {}
extension Double: P {}

public protocol Q {}

public func f1<each T: P>(_ t: repeat each T) -> some Any {
  return (repeat each t)
}

public struct G2<T>: Q {}
public func f2<each T: P>(_ t: repeat each T) -> some Q {
  return G2<(repeat each T)>()
}

public struct G3<each T>: Q {}
public func f3<each T: P>(_ t: repeat each T) -> some Q {
  return G3<repeat each T>()
}
