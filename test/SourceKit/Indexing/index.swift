// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s | %sed_clean | sed -e 's/key.usr: \".*\"/key.usr: <usr>/g' > %t.response
// RUN: diff -u %s.response %t.response

var globV: Int

class CC {
  init() {}
  var instV: CC
  func meth() {}
  func instanceFunc0(_ a: Int, b: Float) -> Int {
    return 0
  }
  func instanceFunc1(a x: Int, b y: Float) -> Int {
    return 0
  }
  class func smeth() {}
}

func +(a : CC, b: CC) -> CC {
  return a
}

struct S {
  func meth() {}
  static func smeth() {}
}

enum E {
  case EElem
}

protocol Prot {
  func protMeth(_ a: Prot)
}

func foo(_ a: CC, b: inout E) {
  globV = 0
  a + a.instV
  a.meth()
  CC.smeth()
  b = E.EElem
  var local: CC
  class LocalCC {}
  var local2: LocalCC
}

typealias CCAlias = CC

extension CC : Prot {
  func meth2(_ x: CCAlias) {}
  func protMeth(_ a: Prot) {}
  var extV : Int { return 0 }
}

class SubCC : CC {}

var globV2: SubCC

class ComputedProperty {
  var value : Int {
    get {
      var result = 0
      return result
    }
    set(newVal) {
      // completely ignore it!
    }
  }

  var readOnly : Int { return 0 }
}

class BC2 {
  func protMeth(_ a: Prot) {}
}
class SubC2 : BC2, Prot {
  override func protMeth(_ a: Prot) {}
}

class CC2 {
  subscript (i : Int) -> Int {
    get {
      return i
    }
    set(v) {
      v+1
    }
  }
}

func test1(_ cp: ComputedProperty, sub: CC2) {
  var x = cp.value
  x = cp.readOnly
  cp.value = x
  ++cp.value
  x = sub[0]
  sub[0] = x
  ++sub[0]
}

struct S2 {
  func sfoo() {}
}

var globReadOnly : S2 {
  get {
    return S2();
  }
}

func test2() {
  globReadOnly.sfoo()
}

class B1 {
  func foo() {}
}

class SB1 : B1 {
  override func foo() {
    foo()
    self.foo()
    super.foo()
  }
}

func test3(_ c: SB1, s: S2) {
  test2()
  c.foo()
  s.sfoo()
}

extension Undeclared {
  func meth() {}
}

class CC4 {
  convenience init(x: Int) {
    self.init(x:0)
  }
}

class SubCC4 : CC4 {
  init(x: Int) {
    super.init(x:0)
  }
}

class Observing {
  init() {}
  var globObserving : Int {
    willSet {
      test2()
    }
    didSet {
      test2()
    }
  }
}

// <rdar://problem/18640140> :: Crash in swift::Mangle::Mangler::mangleIdentifier()
class rdar18640140 {
  // didSet is not compatible with set/get
  var S1: Int {
    get {
      return 1
    }
    set	{
    }
    didSet {
    }
  }
}

protocol rdar18640140Protocol {
  var S1: Int {
    get
    set
    get
  }
}

@available(*, unavailable)
class AlwaysUnavailableClass {
}

@available(iOS 99.99, *)
class ConditionalUnavailableClass1{
}

@available(OSX 99.99, *)
class ConditionalUnavailableClass2{
}
