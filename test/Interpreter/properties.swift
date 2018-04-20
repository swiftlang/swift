// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

var foo: Int {
  get {
    print("foo gotten")
    return 219
  }
  set {
    print("foo set")
  }
}

struct Bar {
  var bar: Int {
    get {
      print("bar gotten")
      return 20721
    }
    set {
      print("bar set")
    }
  }

  static var staticBar: Int {
    get {
      print("staticBar gotten")
      return 97210
    }
    set {
      print("staticBar set")
    }
  }

  static var staticStoredBar: Int = 123456
}

struct WillSetDidSetStruct {
  var x : Int {
    didSet {
      print("got \(x)")
    }
    willSet {
      print("from \(x) to \(newValue)")
    }
  }
  init() {
    x = 0
  }
}

class WillSetDidSetClass {
  var x : Int {
    didSet {
      print("got \(x)")
    }
    willSet {
      print("from \(x) to \(newValue)")
    }
  }
  init() {
    x = 0
  }
}

class DynamicPropertiesBase {
  var x: String { return "base" }
  
  var y : String {
    get {
      return "base"
    }
    set {
      print("set y to \(newValue)")
    }
  }
}
class DynamicPropertiesDerived : DynamicPropertiesBase {
  override var x: String { return "derived" }
}

class DynamicPropertiesObserved : DynamicPropertiesBase {

  override var y : String {
    willSet {
      print("willSet Y from \(y) to \(newValue)!")
    }
    didSet {
      print("didSet Y from \(oldValue) to \(y)!")
    }
  }
}



func test() {
  var b = Bar()
  // CHECK: foo gotten
  // CHECK: 219
  print(foo)
  // CHECK: foo set
  foo = 1

  // CHECK: bar gotten
  // CHECK: 20721
  print(b.bar)
  // CHECK: bar set
  b.bar = 2

  // CHECK: staticBar gotten
  // CHECK: 97210
  print(Bar.staticBar)
  // CHECK: staticBar set
  Bar.staticBar = 3

  // CHECK: 123456
  print(Bar.staticStoredBar)
  Bar.staticStoredBar = 654321
  // CHECK: 654321
  print(Bar.staticStoredBar)
  

  func increment(_ x: inout Int) {
    x += 1
  }

  var ds = WillSetDidSetStruct()
  print("start is \(ds.x)")
  ds.x = 42
  print("now is \(ds.x)")
  increment(&ds.x)
  print("now is \(ds.x)")
  
  // CHECK: start is 0
  // CHECK: from 0 to 42
  // CHECK: got 42
  // CHECK: now is 42
  // CHECK: from 42 to 43
  // CHECK: got 43
  // CHECK: now is 43

  let dsc = WillSetDidSetClass()
  print("start is \(dsc.x)")
  dsc.x = 42
  print("now is \(dsc.x)")
  increment(&dsc.x)
  print("now is \(dsc.x)")
  
  // CHECK: start is 0
  // CHECK: from 0 to 42
  // CHECK: got 42
  // CHECK: now is 42
  // CHECK: from 42 to 43
  // CHECK: got 43
  // CHECK: now is 43


  // Properties should be dynamically dispatched.
  var dpd = DynamicPropertiesDerived()
  print("dpd.x is \(dpd.x)")  // CHECK: dpd.x is derived
  
  var dpb : DynamicPropertiesBase = dpd
  print("dpb.x is \(dpb.x)")  // CHECK: dpb.x is derived

  dpb = DynamicPropertiesBase()
  print("dpb.x is \(dpb.x)")  // CHECK: dpb.x is base

  dpb = DynamicPropertiesObserved()
  dpb.y = "newString"
  // CHECK: willSet Y from base to newString!
  // CHECK: set y to newString
  // CHECK: didSet Y from base to base!
}
test()


/// rdar://16805609 - <rdar://problem/16805609> Providing a 'didSet' in a generic override doesn't work
class rdar16805609Base<T> {
    var value : String = ""
}

class rdar16805609Derived<T> : rdar16805609Base<String>{
    override var value : String {
        didSet(val) {
          print("reached me")
        }
    }
}

let person = rdar16805609Derived<Int>()
print("testing rdar://16805609")    // CHECK: testing rdar://16805609
person.value = "foo"                  // CHECK-NEXT: reached me
print("done rdar://16805609")       // CHECK-NEXT: done rdar://16805609



protocol rdar38514252_ProtocolWithArray {
  var arrayOfInt: [Int] { get set }
}

var rdar38514252_flag = false
var rdar38514252_questionSet: rdar38514252_ProtocolWithArray? {
  didSet {
    rdar38514252_flag = true
  }
}

func rdar38514252_fiddle() {
  let ignored = rdar38514252_questionSet?.arrayOfInt[0]
  if rdar38514252_flag || ignored != nil {
    print("Failed. didSet was called on read.")
  } else {
    print("Awesomesauce.")
  }
}
print("testing rdar://38514252")    // CHECK: testing rdar://38514252
rdar38514252_fiddle()               // CHECK-NEXT: Awesomesauce.
print("done rdar://38514252")       // CHECK-NEXT: done rdar://38514252



// rdar://17192398 - Lazy optional types always nil
class r17192398Failure {
  lazy var i : Int? = 42
  func testLazy() {
    assert(i == 42)
  }
}

let x = r17192398Failure()
x.testLazy()

// <rdar://problem/17226384> Setting a lazy optional property to nil has a strange behavior (Swift)
class r17226384Class {
  lazy var x : Int? = { print("propertyRun"); return 42 }()
}
func test_r17226384() {
  var c = r17226384Class()
  print("created")  // CHECK-NEXT: created
                      // CHECK-NEXT: propertyRun
  print(c.x)        // CHECK-NEXT: Optional(42)
  print("setting")  // CHECK-NEXT: setting
  c.x = nil
  print(c.x)        // CHECK-NEXT: nil
  print("done")     // CHECK-NEXT: done
}
test_r17226384()

class A {}
class HasStaticVar {
  static var a = A()
  static var i = 1010
  static let j = 2020
}

class DerivesHasStaticVar : HasStaticVar {}

assert(HasStaticVar.a === DerivesHasStaticVar.a)
assert(HasStaticVar.i == DerivesHasStaticVar.i)
HasStaticVar.i = 2020
assert(HasStaticVar.i == DerivesHasStaticVar.i)

var _x: Int = 0

class HasClassVar {
  class var x: Int {
    get { return _x }
    set { _x = newValue }
  }
}

assert(HasClassVar.x == 0)
HasClassVar.x += 10
assert(HasClassVar.x == 10)
