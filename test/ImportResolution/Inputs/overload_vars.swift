public var something : Int = 1

public var ambiguousWithVar : Int = 2
public var scopedVar : Int = 3
public var localVar : Int = 4
public var scopedFunction : Int = 5

public var typeNameWins : Int = 6

public protocol HasFoo {
  var foo: Int { get }
}

public protocol HasBar {
  var bar: Int { get }
}

public class HasFooGeneric<T> {
  public var foo: Int = 0
}

extension HasFooGeneric {
  public var bar: Int { return 0 }
}

public class HasFooNonGeneric {
  public var foo: Int = 0
}

extension HasFooNonGeneric {
  public var bar: Int { return 0 }
}
