// RUN: %target-run-simple-swift(-enable-experimental-feature CoroutineAccessors) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_feature_CoroutineAccessors

// Assigning through a `mutate` accessor must not destroy the newly assigned
// value, whatever scope the accessor's self is in: a modify access, the yield
// of a `yielding mutate` accessor, or an inout argument, possibly through a
// projection.

struct Token: ~Copyable {
  let id: Int
  init(_ id: Int) { self.id = id }
  deinit { print("deinit \(id)") }
}

struct Inner: ~Copyable {
  var _t = Token(1)
  var t: Token {
    borrow { return _t }
    mutate { return &_t }
  }
}

struct Mid: ~Copyable {
  var inner = Inner()
  var mutated: Inner {
    borrow { return inner }
    mutate { return &inner }
  }
}

struct Outer: ~Copyable {
  var inner = Inner()
  var mid = Mid()

  var yielded: Inner {
    yielding borrow { yield inner }
    yielding mutate { yield &inner }
  }
  var yieldedMid: Mid {
    yielding borrow { yield mid }
    yielding mutate { yield &mid }
  }

  // Inside these accessors self is an inout argument without an access.
  var innerT: Token {
    borrow { return inner.t }
    mutate { return &inner.t }
  }
  var midInnerT: Token {
    borrow { return mid.inner.t }
    mutate { return &mid.inner.t }
  }
  var midMutatedT: Token {
    borrow { return mid.mutated.t }
    mutate { return &mid.mutated.t }
  }
}

func test(_ name: String, _ body: (inout Outer) -> Void,
          _ stored: (borrowing Outer) -> Int) {
  print(name)
  var o = Outer()
  body(&o)
  print("assigned \(stored(o))")
  _ = consume o
}

// CHECK-LABEL: {{^}}begin_access{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("begin_access", { $0.inner.t = Token(2) }, { $0.inner._t.id })

// CHECK-LABEL: {{^}}begin_access projection{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("begin_access projection", { $0.mid.inner.t = Token(2) },
     { $0.mid.inner._t.id })

// CHECK-LABEL: {{^}}begin_apply{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("begin_apply", { $0.yielded.t = Token(2) }, { $0.inner._t.id })

// CHECK-LABEL: {{^}}begin_apply projection{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("begin_apply projection", { $0.yieldedMid.inner.t = Token(2) },
     { $0.mid.inner._t.id })

// CHECK-LABEL: {{^}}begin_apply nested mutate{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("begin_apply nested mutate", { $0.yieldedMid.mutated.t = Token(2) },
     { $0.mid.inner._t.id })

// CHECK-LABEL: {{^}}self mark{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("self mark", { $0.innerT = Token(2) }, { $0.inner._t.id })

// CHECK-LABEL: {{^}}self mark projection{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("self mark projection", { $0.midInnerT = Token(2) },
     { $0.mid.inner._t.id })

// CHECK-LABEL: {{^}}begin_access nested mutate{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("begin_access nested mutate", { $0.mid.mutated.t = Token(2) },
     { $0.mid.inner._t.id })

// CHECK-LABEL: {{^}}self mark nested mutate{{$}}
// CHECK-NEXT:  deinit 1
// CHECK-NEXT:  assigned 2
test("self mark nested mutate", { $0.midMutatedT = Token(2) },
     { $0.mid.inner._t.id })
