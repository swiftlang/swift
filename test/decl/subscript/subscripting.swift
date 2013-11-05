// RUN: %swift %s -verify

struct X { }

// Simple examples
struct X1 {
  var stored : Int

  subscript (i : Int) -> Int {
    get: return stored
    set: stored = value
  }
}

struct X2 {
  var stored : Int

  subscript (i : Int) -> Int {
    get: return stored + i
    set (val): stored = val - i
  }
}

struct X3 {
  var stored : Int

  subscript (_ : Int) -> Int {
    get: return stored
    set (val): stored = val
  }
}

struct X4 {
  var stored : Int

  subscript (i : Int, j : Int) -> Int {
    get: return stored + i + j
    set (val): stored = val + i - j
  }
}

// Semantic errors
struct Y1 {
  var x : X
  
  subscript (i : Int) -> Int {
    get: return x // expected-error{{'X' is not convertible to 'Int'}}
    set: x = value // expected-error{{'Int' is not convertible to 'X'}}
  }
}

subscript (i : Int) -> Int { // expected-error{{'subscript' functions may only be declared within a type}} expected-error{{subscripting must have a getter}}
}

def f() {
  subscript (i : Int) -> Int { // expected-error{{'subscript' functions may only be declared within a type}} expected-error{{subscripting must have a getter}}
  }
}

struct NoSubscript { }

struct OverloadedSubscript {
  subscript (i : Int) -> Int {
    get: return i
    set:
  }

  subscript (i : Int, j : Int) -> Int {
    get: return i
    set:
  }
}

struct RetOverloadedSubscript {
  subscript (i : Int) -> Int {
    get: return i
    set:
  }

  subscript (i : Int) -> Float {
    get: return Float(i)
    set:
  }
}

struct MissingGetterSubscript {
  subscript (i : Int, j : Int) -> Int { // expected-error{{subscripting must have a getter}}
  }
  subscript (i : Int, j : Int) -> Int { // expected-error{{subscripting must have a getter}}
    set:
  }
}

def test_subscript(x2: X2, i: Int, j: Int, value: Int, no: NoSubscript,
                   ovl: OverloadedSubscript, ret: RetOverloadedSubscript) {
  no[i] = value // expected-error{{'NoSubscript' does not have a member named 'subscript'}}

  value = x2[i]
  x2[i] = value

  value = ovl[i]
  ovl[i] = value

  value = ovl[(i, j)]
  ovl[(i, j)] = value

  value = ovl[(i, j, i)] // expected-error{{expression does not type-check}}

  // FIXME: <rdar://problem/11510876> Implement overload resolution
  ret[i] // expected-error{{expression does not type-check}}

  value = ret[i]
  ret[i] = value
}

def subscript_rvalue_materialize(i: Int) {
  i = X1(0)[i]
}

def subscript_coerce(fn: (String, String) -> Bool) {}
def test_subscript_coerce() {
  subscript_coerce({ $0[$0.length-1] < $1[$1.length-1] })
}

struct no_index {
  subscript () -> Int { return 42 }
  def test() -> Int {
    return self[]
  }
}

struct tuple_index {
  subscript (x : Int, y : Int) -> (Int, Int) { return (x, y) }
  def test() -> (Int, Int) {
    return self[123, 456]
  }
  def test2() -> (Int, Int) {
    return self[y : 123, x : 456]
  }
}
