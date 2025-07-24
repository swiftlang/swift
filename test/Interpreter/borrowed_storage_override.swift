// RUN: %target-run-simple-swift | %FileCheck %s

// Currently, D's opaque read ownership is ::Owned which results in not
// producing an override for C.read.
// XFAIL: *

// REQUIRES: executable_test

protocol P {
  var i: Int { get }
}

class C : P {
  @_borrowed
  var i: Int

  init() {
    self.i = 5
  }
}

class D : C {
  override var i: Int {
    get {42}
    set {}
  }
}

func getFromC(_ c: C) -> Int {
  return c.i
}

func getFromD(_ c: D) -> Int {
  return c.i
}

func getFromP<T : P>(_ c: T) -> Int {
  return c.i
}

func doit() {
  let c = D()
  let ic = getFromC(c)
  print("as C:", ic)
  // CHECK: as C: 42
  let id = getFromD(c)
  print("as D:", id)
  // CHECK: as D: 42
  let ip = getFromP(c)
  print("as P:", ip)
  // CHECK: as P: 42
}

doit()
