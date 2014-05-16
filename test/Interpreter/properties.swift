// RUN: %target-run-simple-swift | FileCheck %s

var foo: Int {
  get {
    println("foo gotten")
    return 219
  }
  set {
    println("foo set")
  }
}

struct Bar {
  var bar: Int {
    get {
      println("bar gotten")
      return 20721
    }
    set {
      println("bar set")
    }
  }

  static var staticBar: Int {
    get {
      println("staticBar gotten")
      return 97210
    }
    set {
      println("staticBar set")
    }
  }

  static var staticStoredBar: Int = 123456
}

struct WillSetDidSetStruct {
  var x : Int {
    didSet {
      println("got \(x)")
    }
    willSet {
      println("from \(x) to \(newValue)")
    }
  }
  init() {
    x = 0
  }
}

struct WillSetDidSetClass {
  var x : Int {
    didSet {
      println("got \(x)")
    }
    willSet {
      println("from \(x) to \(newValue)")
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
      println("set y to \(newValue)")
    }
  }
}
class DynamicPropertiesDerived : DynamicPropertiesBase {
  override var x: String { return "derived" }
}

class DynamicPropertiesObserved : DynamicPropertiesBase {

  override var y : String {
    willSet {
      println("willSet Y from \(y) to \(newValue)!")
    }
    didSet {
      println("didSet Y from \(oldValue) to \(y)!")
    }
  }
}



func test() {
  var b = Bar()
  // CHECK: foo gotten
  // CHECK: 219
  println(foo)
  // CHECK: foo set
  foo = 1

  // CHECK: bar gotten
  // CHECK: 20721
  println(b.bar)
  // CHECK: bar set
  b.bar = 2

  // CHECK: staticBar gotten
  // CHECK: 97210
  println(Bar.staticBar)
  // CHECK: staticBar set
  Bar.staticBar = 3

  // CHECK: 123456
  println(Bar.staticStoredBar)
  Bar.staticStoredBar = 654321
  // CHECK: 654321
  println(Bar.staticStoredBar)
  
  
  var ds = WillSetDidSetStruct()
  println("start is \(ds.x)")
  ds.x = 42
  println("now is \(ds.x)")
  
  // CHECK: start is 0
  // CHECK: from 0 to 42
  // CHECK: got 42
  // CHECK: now is 42

  var dsc = WillSetDidSetClass()
  println("start is \(dsc.x)")
  dsc.x = 42
  println("now is \(dsc.x)")
  
  // CHECK: start is 0
  // CHECK: from 0 to 42
  // CHECK: got 42
  // CHECK: now is 42


  // Properties should be dynamically dispatched.
  var dpd = DynamicPropertiesDerived()
  println("dpd.x is \(dpd.x)")  // CHECK: dpd.x is derived
  
  var dpb : DynamicPropertiesBase = dpd
  println("dpb.x is \(dpb.x)")  // CHECK: dpb.x is derived

  dpb = DynamicPropertiesBase()
  println("dpb.x is \(dpb.x)")  // CHECK: dpb.x is base

  dpb = DynamicPropertiesObserved()
  dpb.y = "newString"
  // CHECK: willSet Y from base to newString!
  // CHECK: set y to newString
  // CHECK: didSet Y from base to base!
}
test()

func lazyInitFunction() -> Int {
  println("lazy property initialized")
  return 0
}


class LazyPropertyClass {
  var id : Int
  @lazy var lazyProperty = lazyInitFunction()

  @lazy var lazyProperty2 : Int = {
    println("other lazy property initialized")
    return 0
  }()


  init(_ ident : Int) {
    id = ident
    println("LazyPropertyClass.init #\(id)")
  }

  deinit {
    println("LazyPropertyClass.deinit #\(id)")
  }
  

}


func testLazyProperties() {
  println("testLazyPropertiesStart") // CHECK: testLazyPropertiesStart
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
  println("testLazyPropertiesDone")    // CHECK: testLazyPropertiesDone
}



testLazyProperties()



/// rdar://16805609 - <rdar://problem/16805609> Providing a 'didSet' in a generic override doesn't work
class rdar16805609Base<T> {
    var value : String = ""
}

class rdar16805609Derived<T> : rdar16805609Base<String>{
    override var value : String {
        didSet(val) {
          println("reached me")
        }
    }
}

let person = rdar16805609Derived<Int>()
println("testing rdar://16805609")    // CHECK: testing rdar://16805609
person.value = "foo"                  // CHECK-NEXT: reached me
println("done rdar://16805609")       // CHECK-NEXT: done rdar://16805609

