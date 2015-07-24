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
