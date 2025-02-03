
public func unspecifiedAsync<T>(_ t: T) async {
}

nonisolated public func nonisolatedAsync<T>(_ t: T) async {
}

public struct S {
  public init() {}
  public func unspecifiedAsync<T>(_ t: T) async {
  }
  
  nonisolated public func nonisolatedAsync<T>(_ t: T) async {
  }
}
