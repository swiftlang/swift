// RUN: %target-typecheck-verify-swift

var global: () -> () = {}

struct Properties {
  var property1: () -> () {
    get {
      return global
    }
    set {
      global = newValue
    }
  }

  var property2: () -> () {
    get {
      return global
    }
    set(value) {
      global = value
    }
  }
}

func testProperties_property1(nonescaping: () -> (), // expected-note {{implicitly non-escaping}}
                              escaping: @escaping () -> ()) {
  var p = Properties()
  p.property1 = nonescaping // expected-error {{assigning non-escaping parameter}}
  p.property1 = escaping
}

func testProperties_property2(nonescaping: () -> (), // expected-note {{implicitly non-escaping}}
                              escaping: @escaping () -> ()) {
  var p = Properties()
  p.property2 = nonescaping // expected-error {{assigning non-escaping parameter}}
  p.property2 = escaping
}

struct Subscripts {
  subscript(value1 fn: Int) -> () -> () {
    get {
      return global
    }
    set {
      global = newValue
    }
  }

  subscript(value2 fn: Int) -> () -> () {
    get {
      return global
    }
    set(value) {
      global = value
    }
  }
  
  subscript(nonescapingIndexWithAddressor fn: () -> Void) -> Int {
    get {
      return 0
    }
    unsafeMutableAddress {
      fatalError()
    }
  }

  // expected-note@+1 2 {{implicitly non-escaping}}
  subscript(nonescapingIndex fn: () -> ()) -> Int {
    get {
      global = fn // expected-error {{assigning non-escaping parameter}}
      return 0
    }
    set {
      global = fn // expected-error {{assigning non-escaping parameter}}
    }
  }

  subscript(escapingIndex fn: @escaping () -> ()) -> Int {
    get {
      global = fn
      return 0
    }
    set {
      global = fn
    }
  }
}

// expected-note@+1 {{implicitly non-escaping}}
func testSubscripts_value1(nonescaping: () -> (),
                           escaping: @escaping () -> ()) {
  var s = Subscripts()
  _ = s[value1: 0]
  s[value1: 0] = escaping
  s[value1: 0] = nonescaping // expected-error {{assigning non-escaping parameter}}
}

// expected-note@+1 {{implicitly non-escaping}}
func testSubscripts_value2(nonescaping: () -> (),
                           escaping: @escaping () -> ()) {
  var s = Subscripts()
  _ = s[value2: 0]
  s[value2: 0] = escaping
  s[value2: 0] = nonescaping // expected-error {{assigning non-escaping parameter}}
}

func testSubscripts_nonescapingIndex(nonescaping: () -> (),
                                     escaping: @escaping () -> ()) {
  var s = Subscripts()
  _ = s[nonescapingIndex: nonescaping]
  _ = s[nonescapingIndex: escaping]
  s[nonescapingIndex: nonescaping] = 0
  s[nonescapingIndex: escaping] = 0
}

// expected-note@+1 2 {{implicitly non-escaping}}
func testSubscripts_escapingIndex(nonescaping: () -> (),
                                  escaping: @escaping () -> ()) {
  var s = Subscripts()
  _ = s[escapingIndex: nonescaping] // expected-error {{passing non-escaping parameter}}
  _ = s[escapingIndex: escaping]
  s[escapingIndex: nonescaping] = 0 // expected-error {{passing non-escaping parameter}}
  s[escapingIndex: escaping] = 0
}
