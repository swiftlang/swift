
public func unspecifiedAsync<T>(_ t: T) async {
}

@execution(caller)
public func unspecifiedAsyncCaller<T>(_ t: T) async {
}

@execution(concurrent)
public func unspecifiedAsyncConcurrent<T>(_ t: T) async {
}

nonisolated public func nonisolatedAsync<T>(_ t: T) async {
}

@execution(caller)
nonisolated public func nonisolatedAsyncCaller<T>(_ t: T) async {
}

@execution(concurrent)
nonisolated public func nonisolatedAsyncConcurrent<T>(_ t: T) async {
}

public struct S {
  public init() {}
  public func unspecifiedAsync<T>(_ t: T) async {
  }

  @execution(caller)
  public func unspecifiedAsyncCaller<T>(_ t: T) async {
  }

  @execution(concurrent)
  public func unspecifiedAsyncConcurrent<T>(_ t: T) async {
  }

  nonisolated public func nonisolatedAsync<T>(_ t: T) async {
  }

  @execution(caller)
  nonisolated public func nonisolatedAsyncCaller<T>(_ t: T) async {
  }

  @execution(concurrent)
  nonisolated public func nonisolatedAsyncConcurrent<T>(_ t: T) async {
  }
}
