// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -verify-additional-prefix deprecated- -enable-experimental-feature DeprecateCompatMemberwiseInit
// REQUIRES: swift_feature_DeprecateCompatMemberwiseInit

struct A {
  // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes 'b'; uses of it will be an error in a future Swift language mode}}
  // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}} {{11:3-3=private init(a: Int, b: Int = 0) {\nself.a = a\nself.b = b\n\}\n}}
  var a: Int
  private var b = 0

  func foo() {
    _ = Self(a: 0, b: 0) // expected-deprecated-note {{memberwise initializer used here}}
  }
}

struct B {
  var a: Int
  private var b = 0

  private init(a: Int, b: Int = 0) {
    self.a = a
    self.b = b
  }

  func foo() {
    _ = Self(a: 0, b: 0)
  }
}

struct C {
  // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes 'b' and 'c'; uses of it will be an error in a future Swift language mode}}
  // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}} {{37:3-3=fileprivate init(a: Int, b: Int = 0, c: Int = 0) {\nself.a = a\nself.b = b\nself.c = c\n\}\n}}
  var a: Int
  fileprivate var b = 0
  fileprivate var c = 0

  func foo() {
    _ = Self(a: 0, b: 0) // expected-deprecated-note {{memberwise initializer used here}}
  }
}

struct D {
  // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes 'b', 'c', and 'd'; uses of it will be an error in a future Swift language mode}}
  // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}} {{50:3-3=private init(a: Int, b: Int = 0, c: Int = 0, d: Int = 0) {\nself.a = a\nself.b = b\nself.c = c\nself.d = d\n\}\n}}
  var a: Int
  private var b = 0
  private var c = 0
  private var d = 0

  func foo() {
    _ = Self(a: 0, b: 0) // expected-deprecated-note {{memberwise initializer used here}}
    _ = D(a: 0, b: 0) // We only diagnose for the first reference
  }
}

struct E {
  // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes private properties with initial values; uses of it will be an error in a future Swift language mode}}
  // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}}

  var a: Int
  private var b = 0
  private var someVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLongName = 0

  func foo() {
    _ = Self(a: 0, b: 0) // expected-deprecated-note {{memberwise initializer used here}}
  }
}

struct F {
  // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes 'c'; uses of it will be an error in a future Swift language mode}}
  // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}} {{82:3-3=fileprivate init(a: Int, c: Int = 0) {\nself.a = a\nself.c = c\n\}\n}}

  var a: Int
  var b: Int
  fileprivate var c: Int = 0 {
    @storageRestrictions(initializes: b)
    init {
      b = newValue
    }
    get { b }
  }
  func foo() {
    _ = Self(a: 0, c: 0) // expected-deprecated-note {{memberwise initializer used here}}
  }
}

struct G {
  var a: Int
  var b: Int

  fileprivate var c: Int = 0 {
    @storageRestrictions(initializes: b)
    init {
      b = newValue
    }
    get { b }
  }

  fileprivate init(a: Int, c: Int = 0) {
    self.a = a
    self.c = c
  }

  func foo() {
    _ = Self(a: 0, c: 0)
  }
}

struct H {
  // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes 'b'; uses of it will be an error in a future Swift language mode}}
  // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}} {{114:1-1=private init(a: Int, b: Int = 0) {\nself.a = a\nself.b = b\n\}\n}}
  var a: Int
  private var b = 0
}

extension H {
  func foo() {
    _ = Self(a: 0, b: 0) // expected-deprecated-note {{memberwise initializer used here}}
  }
}

// We don't currently support accessing a property declared later.
// NOTE: If this restriction is lifted, `emitImplicitValueConstructor` needs
// updating to ensure we initialize `d` before `b`.
struct I { // expected-error {{cannot synthesize memberwise initializer}}
  var a: Int
  var b: Int
  private var c: Int = 0 {
    @storageRestrictions(initializes: b, accesses: d)
    init { b = newValue + d }
    // expected-note@-1 {{init accessor for 'c' cannot access stored property 'd' because it is called before 'd' is initialized}}
    get { b }
  }
  var d: Int
}

protocol P {
  init(x: Int?, y: Int)
}

private struct TestNested {
  struct S: P {
    fileprivate var x: Int?
    var y: Int
  }
}

func testLocal() {
  struct A: P {
    fileprivate var x: Int?
    var y: Int
  }
  struct B {
    // expected-deprecated-warning@-1 {{synthesized memberwise initializer no longer includes 'x'; uses of it will be an error in a future Swift language mode}}
    // expected-deprecated-note@-2 {{insert an explicit implementation of the memberwise initializer}} {{159:5-5=private init(x: Int? = nil, y: Int) {\nself.x = x\nself.y = y\n\}\n}}
    private var x: Int?
    var y: Int

    func foo() {
      _ = B(x: 0, y: 0) // expected-deprecated-note {{memberwise initializer used here}}
    }
  }
}
