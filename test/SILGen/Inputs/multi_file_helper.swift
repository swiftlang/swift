public struct Range {
  public var loc, length: Int
}

extension Range {
  // This specifically needs to be in an extension to tickle
  // <rdar://problem/16016713>.
  public var limit: Int {
    return loc + length
  }
}

struct LazyContainer {
  lazy var lazyVar = 42
}

class LazyContainerClass {
  lazy var lazyVar = 42
}

class FinalPropertyClass {
  final var foo = 50
  var bar = 55
}

final class ObservingPropertyFinalClass {
  var foo: Int {
    didSet {}
  }
  init(_ foo: Int) { self.foo = foo }
}

protocol ProtocolWithProperty {
  var foo: Int { get set }
}
