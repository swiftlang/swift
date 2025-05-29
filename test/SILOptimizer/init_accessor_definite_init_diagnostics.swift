// RUN: %target-swift-frontend -sil-verify-all -enable-copy-propagation=requested-passes-only -emit-sil -primary-file %s -o /dev/null -verify

struct Test1 {
  var x: Int // expected-note {{variable defined here}}
  var y: Int // expected-note {{variable defined here}}
  var full: (Int, Int)

  var test1: (Int, Int) {
    @storageRestrictions(initializes: y, full, accesses: x)
    init(initialValue) {
      self.full = (self.x, self.y) // expected-error {{variable 'y' used before being initialized}}
    }

    get { full }
    set { self.full = newValue }
  }

  var pointY: Int {
    @storageRestrictions(initializes: y)
    init(initialValue) {
      self.y = initialValue // Ok
    }

    get { y }
    set {}
  }

  var errorPoint1: (Int, Int) {
    @storageRestrictions(initializes: x, y)
    init(initialValue) {
      // expected-error@-1 {{property 'x' not initialized by init accessor}}
      // expected-error@-2 {{property 'y' not initialized by init accessor}}
    }

    get { (x, y) }
    set { }
  }

  var errorPoint2: (Int, Int) {
    @storageRestrictions(initializes: x, y)
    init(initialValue) {
      // expected-error@-1 {{property 'y' not initialized by init accessor}}
      self.x = initialValue.0
    }

    get { (x, y) }
    set { }
  }

  var errorPoint3: (Int, Int) {
    @storageRestrictions(initializes: x, y)
    init(initialValue) {
      self.y = initialValue.1
      print(y) // Ok
      print(x) // expected-error {{variable 'x' used before being initialized}}
    }

    get { (x, y) }
    set { }
  }

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
    self.full = (x, y)
  }
}

struct TestPartial {
  var x: Int
  var y: Int

  var point: (Int, Int) {
    @storageRestrictions(initializes: x, y)
    init(initialValue) {
      self.x = initialValue.0
      self.y = initialValue.1
    }

    get { (x, y) }
    set { }
  }

  init(x: Int, y: Int) {
    self.x = x
    self.point = (x, y) // Ok (x is going to get `destroy_addr`)
  }

  init(_ x: Int, _ y: Int) {
    self.x = x
    self.y = y
    self.point = (x, y) // Ok (calls a setter)
  }
}

struct TestDoubleInit1 {
  let x: Int // expected-note {{change 'let' to 'var' to make it mutable}}

  var invalidPointX: Int {
    @storageRestrictions(initializes: x)
    init(initialValue) {
      self.x = initialValue
      self.x = 42 // expected-error {{immutable value 'x' may only be initialized once}}
    }

    get { x }
    set { }
  }
}

struct TestDoubleInit2 {
  let x: Int // expected-note {{change 'let' to 'var' to make it mutable}}

  var pointX: Int {
    @storageRestrictions(initializes: x)
    init(initialValue) {
      self.x = initialValue
    }

    get { x }
    set { }
  }

  init(x: Int) {
    self.pointX = x
    self.x = 0 // expected-error {{immutable value 'self.x' may only be initialized once}}
  }
}

struct TestAccessBeforeInit {
  var _x: Int
  var x: Int {
    @storageRestrictions(initializes: _x, accesses: y)
    init(initialValue) {
      _x = initialValue
    }

    get { _x }
    set {}
  }

  var y: Int

  init(x: Int, y: Int) {
    self.x = x // expected-error {{variable 'self.y' used before being initialized}}
    self.y = y
  }
}

class TestInitWithGuard {
  var _a: Int
  var _b: Int

  var pair1: (Int, Int) {
    @storageRestrictions(initializes: _a, _b)
    init(initialValue) { // expected-error {{property '_b' not initialized by init accessor}}
      _a = initialValue.0

      if _a > 0 {
        return
      }

      _b = initialValue.1
    }

    get { (_a, _b) }
    set { }
  }

  var pair2: (Int, Int) {
    @storageRestrictions(initializes: _a, _b)
    init(initialValue) { // Ok
      _a = initialValue.0

      if _a > 0 {
        _b = 0
        return
      }

      _b = initialValue.1
    }

    get { (_a, _b) }
    set { }
  }

  init(a: Int, b: Int) {
    self.pair2 = (a, b)
  }
}

do {
  struct TestPropertyInitWithUnreachableBlocks {
    var _a: Int
    var a: Int? {
      @storageRestrictions(initializes: _a)
      init { _a = newValue ?? 0 } // Ok
      get { 42 }
    }
  }
}

do {
  class Base<T: Collection> {
    private var _v: T

    var data: T {
      @storageRestrictions(initializes: _v)
      init(initialValue) {
        _v = initialValue
      }

      get { _v }
    }

    init(data: T) {
      self.data = data
    }

    init(reinit: T) {
      self.data = reinit
      self.data = reinit // expected-error {{immutable value 'data' may only be initialized once}}
    }
  }

  class Sub<U> : Base<U> where U: Collection, U.Element == String {
    init(other: U) {
      super.init(data: other)
    }

    init(error: U) {
      super.init(data: error)
      data = error // expected-error {{immutable value 'data' may only be initialized once}}
    }
  }

  // Make sure that re-initialization is not allowed when there is no setter.
  struct TestPartialWithoutSetter {
    var _a: Int

    var a: Int {
      @storageRestrictions(initializes: _a)
      init(initialValue) {
        self._a = initialValue
      }

      get { _a }
    }

    var b: Int

    init(v: Int) {
      self.a = v
      self.a = v // expected-error {{immutable value 'a' may only be initialized once}}
      self.b = v
    }
  }
}

do {
  class Entity {
    var _age: Int
    var age: Int = 0 {
      @storageRestrictions(initializes: _age)
      init { _age = newValue }
      get { _age }
      set { _age = newValue }
    }
  }

  class Person : Entity {
    init(age: Int) {
      self.age = age // expected-error {{'self' used in property access 'age' before 'super.init' call}}
    }

    init(otherAge: Int) {
      super.init()
      self.age = otherAge // Ok
    }
  }
}

// Test that init accessor without "initializes" is required
do {
  struct Test {
    var a: Int {
      init {}
      get { 42 }
      set {}
    }
    var b: Int { // expected-note 2 {{'self' not initialized}}
      init {}
      get { 0 }
      set { }
    }

    init(a: Int) {
      self.a = a
    } // expected-error {{return from initializer without initializing all stored properties}}

    init(a: Int, b: Int) {
      if a == 0 {
        self.a = a
        self.b = b
      } else {
        self.a = 0
      }
    } // expected-error {{return from initializer without initializing all stored properties}}

    init() { // Ok
      self.a = 0
      self.b = 0
    }
  }

  struct TestWithStored {
    var _value: Int = 0

    var a: Int {
      @storageRestrictions(accesses: _value)
      init {}
      get { _value }
      set { _value = newValue }
    }
  }

  _ = TestWithStored(a: 42) // Ok
  _ = TestWithStored(_value: 1, a: 42) // Ok

  class TestWithStoredExplicit {
    var _value: Int = 0
    var _other: String = ""

    var a: Int { // expected-note 2 {{'self' not initialized}}
      @storageRestrictions(accesses: _value)
      init {}
      get { _value }
      set { _value = newValue }
    }

    init(data: Int) {
      self._value = data
    } // expected-error {{return from initializer without initializing all stored properties}}

    init() {} // expected-error {{return from initializer without initializing all stored properties}}

    init(a: Int) {
      self.a = a // Ok
    }
  }
}
