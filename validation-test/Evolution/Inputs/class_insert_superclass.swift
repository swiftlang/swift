open class Root {}

#if BEFORE

open class FirstMiddle : Root {
  let x: String

  public init(x: String) {
    self.x = x
  }

  public func get() -> String {
    return x
  }
}

open class SecondMiddle : Root {
  let x: String

  public init(x: String) {
    self.x = x
  }

  public func get() -> String {
    return x
  }
}

open class GenericMiddle<T> : Root {
  let x: T

  public init(x: T) {
    self.x = x
  }

  public func get() -> T {
    return x
  }
}

#else

// Insert concrete superclass
open class Base : Root {
  let x: String

  public init(t: String) {
    self.x = t
  }
}

open class FirstMiddle : Base {
  public init(x: String) {
    super.init(t: x)
  }

  public func get() -> String {
    return x
  }
}

// Insert generic superclass
open class GenericBase<T> : Root {
  let x: T

  public init(t: T) {
    self.x = t
  }
}

open class SecondMiddle : GenericBase<String> {
  public init(x: String) {
    super.init(t: x)
  }

  public func get() -> String {
    return x
  }
}

// Insert concrete superclass - class itself is generic
open class GenericMiddle<T> : GenericBase<T> {
  public init(x: T) {
    super.init(t: x)
  }

  public func get() -> T {
    return x
  }
}

#endif
