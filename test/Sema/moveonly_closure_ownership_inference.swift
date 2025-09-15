// RUN: %target-swift-frontend %s -emit-sil -verify

struct NC: ~Copyable {
  func borrow() {}
  consuming func consume() {}
  mutating func mutate () {}
}

struct Box<Wrapped: ~Copyable> {
  consuming func map(_ transform: (consuming Wrapped)->Void) {  }
}

struct Tree: ~Copyable {
  static func insert(_ box: consuming Box<Self>) {
    box.map { x  in x.insert() }
  }

  consuming func insert() {  }
}

func withConsuming(_ body: (consuming NC) -> Void) {
  body(NC())
}

func withBorrowing(_ body: (borrowing NC) -> Void) {
  body(NC())
}

func withMutating(_ body: (inout NC) -> Void) {
  var nc = NC()
  body(&nc)
}

func withShared(_ body: (__shared NC) -> Void) {
  body(NC())
}

func withOwned(_ body: (__owned NC) -> Void) {
  body(NC())
}

withMutating { nc in
    nc.mutate()
}

withConsuming { nc in
    nc.consume()
}

withBorrowing { // expected-error {{'$0' is borrowed and cannot be consumed}}
  $0.borrow()
  $0.consume() // expected-note {{consumed here}}
}

withShared { // expected-error {{'$0' is borrowed and cannot be consumed}}
  $0.borrow()
  $0.consume() // expected-note {{consumed here}}
}

withOwned {
  $0.consume()
}

do {
  let clos: (consuming NC) -> Void = {
    $0.consume()
  }
  clos(NC())
}
