// RUN: not %target-swift-frontend -typecheck %s

protocol P: class { }

protocol Q {
  func g()
}

protocol P { }

struct S : Q {
  @_implements(P, g())
  func h() {}
}

