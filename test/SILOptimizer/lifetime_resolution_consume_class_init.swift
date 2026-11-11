// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -verify %s -o /dev/null

// The stored properties of `self` in a class initializer may be consumed, after which
// they must be reinitialized before `self` is used as a whole.

class Tree {}
struct E: Error {}

func use(_ t: Tree?) {}
func observe(_ x: AnyObject) {}
func mayThrow() throws {}

class ReinitAfterConsume {
  var left: Tree?
  var right: Tree?
  init() {
    left = Tree()
    right = nil
    _ = consume right
    right = Tree()
    observe(self)
  }
}

class UseSelfAfterConsume {
  var left: Tree?
  var right: Tree?
  init() {
    left = Tree()
    right = nil
    _ = consume right // expected-note {{partially consumed here}} // expected-note {{write 'copy self.right' to keep 'self.right' valid after this consume}}
    observe(self) // expected-error {{cannot use 'self' after partial consume}}
  }
}

class UseFieldAfterConsume {
  var left: Tree?
  var right: Tree?
  init() {
    left = Tree()
    right = nil
    _ = consume left // expected-note {{consumed here}} // expected-note {{write 'copy self.left' to keep 'self.left' valid after this consume}}
    use(left) // expected-error {{'self.left' used after consume}}
    use(right)
    left = nil
  }
}

class ReturnAfterConsume {
  var left: Tree?
  var right: Tree?
  init() {
    left = Tree()
    right = nil
    _ = consume left // expected-note {{partially consumed here}} // expected-note {{write 'copy self.left' to keep 'self.left' valid after this consume}}
  } // expected-error {{cannot use 'self' after partial consume}}
}

class MethodCallAfterConsume {
  var left: Tree?
  var right: Tree?
  init() {
    left = Tree()
    right = nil
    _ = consume left // expected-note {{partially consumed here}} // expected-note {{write 'copy self.left' to keep 'self.left' valid after this consume}}
    method() // expected-error {{cannot use 'self' after partial consume}}
    left = nil
  }
  func method() {}
}

class CaptureAfterConsume {
  var left: Tree?
  var right: Tree?
  init(_ b: Bool) {
    left = Tree()
    right = Tree()
    if b {
      _ = consume left // expected-note {{partially consumed here}} // expected-note {{write 'copy self.left' to keep 'self.left' valid after this consume}}
    } else {
      _ = consume right // expected-note {{partially consumed here}} // expected-note {{write 'copy self.right' to keep 'self.right' valid after this consume}}
    }
    let f = { _ = self } // expected-error {{cannot use 'self' after partial consume}}
    f()
  }
}

class ConsumeInLoop {
  var left: Tree?
  var right: Tree?
  init(_ b: Bool) {
    left = Tree()
    right = nil
    while b {
      _ = consume left // expected-error {{'self.left' consumed in a loop}} // expected-note {{write 'copy self.left' to keep 'self.left' valid after this consume}}
    }
    left = nil
  }
}

class ConsumeTwice {
  var left: Tree?
  var right: Tree?
  init() {
    left = Tree()
    right = nil
    _ = consume left // expected-note {{consumed here}} // expected-note {{write 'copy self.left' to keep 'self.left' valid after this consume}}
    _ = consume left // expected-error {{'self.left' used after consume}}
    left = nil
  }
}

// A partially initialized `self` is cleaned up when the initializer fails, so its
// consumed stored properties need not be reinitialized beforehand.
class ThrowBeforeReinit {
  var left: Tree?
  var right: Tree?
  init() throws {
    left = Tree()
    right = nil
    _ = consume left
    try mayThrow()
    left = nil
  }
}

class ThrowWithoutReinit {
  var left: Tree?
  var right: Tree?
  init() throws {
    left = Tree()
    right = nil
    _ = consume left
    throw E()
  }
}

class FailWithoutReinit {
  var left: Tree?
  var right: Tree?
  init?(_ b: Bool) {
    left = Tree()
    right = nil
    _ = consume left
    if b { return nil }
    left = nil
  }
}

class Generic<T> {
  var t: T
  var u: T
  init(_ x: T) {
    t = x
    u = x
    _ = consume t // expected-note {{partially consumed here}} // expected-note {{write 'copy self.t' to keep 'self.t' valid after this consume}}
    print(self) // expected-error {{cannot use 'self' after partial consume}}
  }
}

class Base {
  init() {}
}

class DerivedReinitBeforeSuper: Base {
  var t: Tree?
  override init() {
    t = Tree()
    _ = consume t
    t = Tree()
    super.init()
    observe(self)
  }
}

class DerivedConsumeBeforeSuper: Base {
  var t: Tree?
  override init() {
    t = Tree()
    _ = consume t // expected-note {{partially consumed here}} // expected-note {{write 'copy self.t' to keep 'self.t' valid after this consume}}
    super.init() // expected-error {{cannot use 'self' after partial consume}}
  }
}

class DerivedReinitAfterSuper: Base {
  var t: Tree?
  override init() {
    t = Tree()
    super.init()
    _ = consume t
    t = Tree()
    observe(self)
  }
}

class DerivedUseSelfAfterConsume: Base {
  var t: Tree?
  override init() {
    t = Tree()
    super.init()
    _ = consume t // expected-note {{partially consumed here}} // expected-note {{write 'copy self.t' to keep 'self.t' valid after this consume}}
    observe(self) // expected-error {{cannot use 'self' after partial consume}}
  }
}

// FIXME: this should be diagnosed. After `super.init`, `self` can't be deallocated without running the
// deinit of its superclass, yet its deinit would destroy the stored property that was consumed.
class DerivedThrowAfterConsume: Base {
  var t: Tree?
  init(fail: Bool) throws {
    t = Tree()
    super.init()
    _ = consume t
    if fail { throw E() }
    t = Tree()
  }
}
