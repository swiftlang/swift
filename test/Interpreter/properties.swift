// RUN: %target-run-simple-swift | FileCheck %s

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

struct WillSetDidSetClass {
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
  
  
  var ds = WillSetDidSetStruct()
  print("start is \(ds.x)")
  ds.x = 42
  print("now is \(ds.x)")
  
  // CHECK: start is 0
  // CHECK: from 0 to 42
  // CHECK: got 42
  // CHECK: now is 42

  var dsc = WillSetDidSetClass()
  print("start is \(dsc.x)")
  dsc.x = 42
  print("now is \(dsc.x)")
  
  // CHECK: start is 0
  // CHECK: from 0 to 42
  // CHECK: got 42
  // CHECK: now is 42


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

func lazyInitFunction() -> Int {
  print("lazy property initialized")
  return 0
}


class LazyPropertyClass {
  var id : Int
  lazy var lazyProperty = lazyInitFunction()

  lazy var lazyProperty2 : Int = {
    print("other lazy property initialized")
    return 0
  }()


  init(_ ident : Int) {
    id = ident
    print("LazyPropertyClass.init #\(id)")
  }

  deinit {
    print("LazyPropertyClass.deinit #\(id)")
  }
  

}


func testLazyProperties() {
  print("testLazyPropertiesStart") // CHECK: testLazyPropertiesStart
  if true {
    var a = LazyPropertyClass(1)      // CHECK-NEXT: LazyPropertyClass.init #1
    _ = a.lazyProperty                // CHECK-NEXT: lazy property initialized
    _ = a.lazyProperty    // executed only once, lazy init not called again.
    a.lazyProperty = 42   // nothing interesting happens
    _ = a.lazyProperty2               // CHECK-NEXT: other lazy property initialized

    // CHECK-NEXT: LazyPropertyClass.init #2
    // CHECK-NEXT: LazyPropertyClass.deinit #1
    a = LazyPropertyClass(2)

    a = LazyPropertyClass(3)
    a.lazyProperty = 42   // Store don't trigger lazy init.

    // CHECK-NEXT: LazyPropertyClass.init  #3
    // CHECK-NEXT: LazyPropertyClass.deinit #2
    // CHECK-NEXT: LazyPropertyClass.deinit #3
  }
  print("testLazyPropertiesDone")    // CHECK: testLazyPropertiesDone
}



testLazyProperties()



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




// rdar://17192398 - Lazy optional types always nil
class r17192398Failure {
  lazy var i : Int? = 42
  func testLazy() {
    assert(i == 42)
  }
}

let x = r17192398Failure()
x.testLazy()

// <rdar://problem/17226384> Setting an lazy optional property to nil has a strange behavior (Swift)
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
