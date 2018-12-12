public var count = 0

public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

open class Base {
  public init() {}

#if AFTER
  deinit {
    count += 1
  }
#endif
}

open class Derived : Base {
  deinit {
    count += 10
  }
}
