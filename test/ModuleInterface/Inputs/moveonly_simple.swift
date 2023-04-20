public enum SwiftCoin : ~Copyable {
  case penny
  case nickel

  init() { self = .penny }
}

// ensures old attribute still works.
@_moveOnly public enum ObjCCoin {
  case penny
  case nickel

  init() { self = .penny }
}

