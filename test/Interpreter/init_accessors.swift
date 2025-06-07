// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

struct TestInit {
  var x: Int
  var y: Int
  var full: (Int, Int)

  var point: (Int, Int) {
    @storageRestrictions(initializes: y, full, accesses: x)
    init(initialValue) {
      self.y = initialValue.1
      self.full = (self.x, self.y)
    }

    get { full }
    set { full = newValue }
  }

  init(x: Int, y: Int) {
    self.x = x
    self.point = (x, y)
  }
}

do {
  let test = TestInit(x: 0, y: -1)
  print("test-init: \(test.point)")
  // CHECK: test-init: (0, -1)
}

struct TestSetter {
  var x: Int
  var y: Int

  var point: (Int, Int) {
    @storageRestrictions(accesses: x, y)
    init(initialValue) {
    }

    get { (x, y) }
    set { }
  }

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
    self.point = (x, y)
  }
}

do {
  let test = TestSetter(x: 0, y: -2)
  print("test-setter: \(test.point)")
  // CHECK: test-setter: (0, -2)
}

struct TestInitThenSetter {
  var x: Int
  var y: Int

  var point: (Int, Int) {
    @storageRestrictions(initializes: x, y)
    init(initialValue) {
      self.x = initialValue.0
      self.y = initialValue.1
    }

    get { (x, y) }

    set {
      x = newValue.0
      y = newValue.1
    }
  }

  init(x: Int, y: Int) {
    self.point = (x, y)

    if x == 1 {
      self.point = (0, 0)
    }
  }
}

do {
  let test = TestInitThenSetter(x: 1, y: 2)
  print("test-init-then-setter: \(test.point)")
  // CHECK: test-init-then-setter: (0, 0)
}

struct TestPartialInt {
  var x: Int
  var y: Int

  var pointX: Int {
    @storageRestrictions(initializes: x)
    init(newValue) {
      self.x = newValue
    }

    get { x }
    set { self.x = newValue }
  }

  var pointY: Int {
    @storageRestrictions(initializes: y)
    init(newValue) {
      self.y = newValue
    }

    get { y }
    set { self.y = newValue }
  }

  init(x: Int, y: Int) {
    // Init
    self.pointX = x
    // Init
    self.pointY = y

    // Setter
    self.pointX = 1
    // Setter
    self.pointY = 2
  }
}

do {
  let test = TestPartialInt(x: 0, y: -1)
  print("test-partial-init: (\(test.pointX), \(test.pointY))")
  // CHECK: test-partial-init: (1, 2)
}

struct TestNoInitAndInit {
  var x: Int
  var y: Int

  var pointX: Int {
    @storageRestrictions(accesses: x)
    init(initalValue) {
    }

    get { x }
    set { }
  }

  var pointY: Int {
    @storageRestrictions(initializes: y)
    init(initialValue) {
      self.y = initialValue
    }

    get { y }
    set { }
  }

  init(x: Int, y: Int) {
    self.x = x
    self.pointX = x
    self.pointY = y
    print("TestNoInitAndInit(x: \(self.x), y: \(self.y))")
  }
}

do {
  _ = TestNoInitAndInit(x: 10, y: -10)
  // CHECK: TestNoInitAndInit(x: 10, y: -10)
}

class TestClass {
  var x: Int
  var y: (Int, [String])

  var data: (Int, (Int, [String])) {
    @storageRestrictions(initializes: x, y)
    init(initialValue) {
      x = initialValue.0
      y = initialValue.1
    }

    get { (x, y) }
    set {
      x = newValue.0
      y = newValue.1
    }
  }

  init(x: Int, y: (Int, [String])) {
    self.data = (x, y)
  }
}

do {
  let test = TestClass(x: 20, y: (0, ["a", "b"]))
  print("test-class: \(test.data)")
  // CHECK: test-class: (20, (0, ["a", "b"]))
}

struct TestGeneric<T, U> {
  var a: T
  var b: T
  var c: U

  var data: (T, T) {
    @storageRestrictions(initializes: a, b, accesses: c)
    init(initialValue) {
      a = initialValue.0
      b = initialValue.1
      print("TestGeneric(c: \(c))")
    }

    get { (a, b) }
    set { }
  }

  init(a: T, b: T, c: U) {
    self.c = c
    self.data = (a, b)
    self.data = (b, a)
  }
}

do {
  let test = TestGeneric(a: 42, b: 0, c: [42, "a"] as [Any])
  print("test-generic: data = \(test.data)")
  // CHECK: TestGeneric(c: [42, "a"])
  // CHECK-NEXT: test-generic: data = (42, 0)
}

func test_local_with_memberwise() {
  class MyValue {}

  struct TestMemberwiseConcrete {
    var a: Int
    var b: String

    var pair: (Int, String) {
      @storageRestrictions(initializes: a, b)
      init(initialValue) {
        a = initialValue.0
        b = initialValue.1
      }

      get { (a, b) }
      set { }
    }

    var c: [MyValue]
  }

  let concrete = TestMemberwiseConcrete(pair: (0, "a"), c: [])
  print(concrete)

  struct TestMemberwiseGeneric<T, C> where C: RangeReplaceableCollection, C.Element == T {
    var _a: T
    var _b: String
    var _c: C

    var a: T {
      @storageRestrictions(initializes: _a)
      init(initialValue) {
        _a = initialValue
      }

      get { _a }
      set { }
    }

    var pair: (String, C) {
      @storageRestrictions(initializes: _b, _c, accesses: _a)
      init(initialValue) {
        _b = initialValue.0
        _c = initialValue.1
        _c.append(_a)
      }

      get { (_b, _c) }
      set { }
    }
  }

  let generic = TestMemberwiseGeneric(a: 1, pair: ("a", [0]))
  print(generic)
}

test_local_with_memberwise()
// CHECK: TestMemberwiseConcrete(a: 0, b: "a", c: [])
// CHECK-NEXT: TestMemberwiseGeneric<Int, Array<Int>>(_a: 1, _b: "a", _c: [0, 1])

func test_assignments() {
  struct Test {
    var _a: Int
    var _b: Int

    var a: Int {
      @storageRestrictions(initializes: _a)
      init(initialValue) {
        self._a = initialValue
        print("a-init-accessor: \(self._a)")
      }
      get { _a }
      set { _a = newValue + 1 }
    }

    var pair: (Int, Int) {
      @storageRestrictions(initializes: _a, _b)
      init(initialValue) {
        _a = initialValue.0
        _b = initialValue.1
      }

      get { (_a, _b) }
      set { }
    }

    init(a: Int) {
      // init
      self.a = a
      // re-assignment
      self.a = a + 1
      self._b = 42
      // set
      self.a = a + 2
    }

    init(a: Int, b: Int) {
      self.a = a
      self.pair = (0, b)
    }
  }

  let test1 = Test(a: 0)
  print("test-assignments-1: \(test1.pair)")

  let test2 = Test(a: 0, b: 2)
  print("test-assignments-2: \(test2.pair)")
}

test_assignments()
// CHECK: a-init-accessor: 0
// CHECK-NEXT: a-init-accessor: 1
// CHECK-NEXT: test-assignments-1: (3, 42)
// CHECK-NEXT: a-init-accessor: 0
// CHECK-NEXT: test-assignments-2: (0, 2)

func test_memberwise_ordering() {
  struct Test1 {
    var _a: Int
    var _b: Int

    var a: Int {
      @storageRestrictions(initializes: _a, accesses: _b)
      init(initialValue) {
        _a = initialValue
      }

      get { _a }
      set { }
    }
  }

  let test1 = Test1(_b: 42, a: 0)
  print("test-memberwise-ordering-1: \(test1)")

  struct Test2 {
    var _a: Int

    var pair: (Int, Int) {
      @storageRestrictions(initializes: _a, _b)
      init(initialValue) {
        _a = initialValue.0
        _b = initialValue.1
      }

      get { (_a, _b) }
      set { }
    }

    var _b: Int
  }

  let test2 = Test2(pair: (-1, -2))
  print("test-memberwise-ordering-2: \(test2)")

  struct Test3 {
    var _a: Int
    var _b: Int

    var pair: (Int, Int) {
      @storageRestrictions(accesses: _a, _b)
      init(initialValue) {
      }

      get { (_a, _b) }
      set { }
    }

    var _c: Int
  }

  let test3 = Test3(_a: 1, _b: 2, pair: (1, 2), _c: 3)
  print("test-memberwise-ordering-3: \(test3)")
}

test_memberwise_ordering()
// CHECK: test-memberwise-ordering-1: Test1(_a: 0, _b: 42)
// CHECK-NEXT: test-memberwise-ordering-2: Test2(_a: -1, _b: -2)
// CHECK-NEXT: test-memberwise-ordering-3: Test3(_a: 1, _b: 2, _c: 3)

func test_memberwise_with_default_args() {
  struct TestWithoutDefault {
    var _a: Int
    var _b: Int

    var pair: (Int, Int) = (-1, 42) {
      @storageRestrictions(initializes: _a, _b)
      init(initialValue) {
        _a = initialValue.0
        _b = initialValue.1
      }

      get { (0, 42) }
      set { }
    }
  }

  let test1 = TestWithoutDefault()
  print("test-memberwise_with_default-1: \(test1)")

  let test2 = TestWithoutDefault(pair: (42, -1))
  print("test-memberwise_with_default-2: \(test2)")

  struct TestDefaulted {
    var _a: Int = 0
    var _b: Int = 0

    var pair: (Int, Int) = (1, 2) {
      @storageRestrictions(initializes: _a, _b)
      init(initialValue) {
        _a = initialValue.0
        _b = initialValue.1
      }

      get { (_a, _b) }
      set { }
    }
  }

  let test3 = TestDefaulted()
  print("test-defaulted-1: \(test3)")

  let test4 = TestDefaulted(pair: (3, 4))
  print("test-defaulted-2: \(test4)")

  class TestClass {
    var _q: String = "<<default>>"
    var _a: Int = 1

    var pair: (String, Int) = ("", 42) {
      @storageRestrictions(initializes: _q, _a)
      init(initialValue) {
        _q = initialValue.0
        _a = initialValue.1
      }

      get { (_q, _a) }
      set { }
    }
  }

  let test5 = TestClass()
  print("test-defaulted-class: \(test5.pair)")
}

test_memberwise_with_default_args()
// CHECK: test-memberwise_with_default-1: TestWithoutDefault(_a: -1, _b: 42)
// CHECK-NEXT: test-memberwise_with_default-2: TestWithoutDefault(_a: 42, _b: -1)
// CHECK-NEXT: test-defaulted-1: TestDefaulted(_a: 1, _b: 2)
// CHECK-NEXT: test-defaulted-2: TestDefaulted(_a: 3, _b: 4)
// CHECK-NEXT: test-defaulted-class: ("", 42)

func test_init_accessors_without_setters() {
  struct TestStruct<T> {
    var _x: T

    var x: T {
      @storageRestrictions(initializes: _x)
      init(initialValue) {
        _x = initialValue
      }

      get { _x }
    }

    init(value: T) {
      x = value
    }
  }

  let test1 = TestStruct(value: 42)
  print("test-without-setter1: \(test1.x)")

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
  }

  let test2 = Base(data: [1, 2, 3])
  print("test-without-setter2: \(test2.data)")

  class Sub<U> : Base<U> where U: Collection, U.Element == String {
    init(other: U) {
      super.init(data: other)
    }
  }

  let test3 = Sub(other: ["a", "b", "c"])
  print("test-without-setter3: \(test3.data)")
}

test_init_accessors_without_setters()
// CHECK: test-without-setter1: 42
// CHECK-NEXT: test-without-setter2: [1, 2, 3]
// CHECK-NEXT: test-without-setter3: ["a", "b", "c"]

func test_effects_are_still_supported() {
  struct Test {
    var _a: Int
    var _b: Int

    var a: Int {
      @storageRestrictions(initializes: _a, accesses: _b)
      init(initialValue) {
        _a = initialValue
        _b = 0
      }

      get { _a }
    }
  }

  let test = Test(_b: 1, a: 42)
  print("effects-support-test: \(test)")
}

test_effects_are_still_supported()
// CHEKC: effects-support-test: Test(_a: 42, b: 0)

func test_memberwise_without_stored_properties() {
  struct Test {
    var a: Int {
      init {
        print("no-stored: a = \(newValue)")
      }

      get { 0 }
    }

    var b: Int {
      init {
        print("no-stored: b = \(newValue)")
      }

      get { 1 }
    }
  }

  _ = Test(a: 1, b: 2)
}

test_memberwise_without_stored_properties()
// CHECK: no-stored: a = 1
// CHECK-NEXT: no-stored: b = 2

protocol P {
  static var initialValue: Self { get }
}

func test_properties_with_inits() {
  struct S: P, CustomStringConvertible {
    var x: Int

    static var initialValue: S { S(x: 42) }

    var description: String {
      "S(x: \(x))"
    }
  }

  final class K: P, CustomStringConvertible {
    var v: [String]
    static var initialValue: K { K(v: ["question"]) }

    var description: String {
      "K(v: \(v))"
    }

    init(v: [String]) {
      self.v = v
    }
  }

  class Test<T: P> {
    var _x: T

    var x: T = T.initialValue {
      @storageRestrictions(initializes: _x)
      init {
        _x = newValue
      }
      get { _x }
      set { _x = newValue }
    }

    init() {}
  }

  print("test-init-expr-1: \(Test<S>().x)")

  struct TestPair<T: P, U: P> {
    var _data: (T, U)

    var data: (T, U) = (T.initialValue, U.initialValue) {
      @storageRestrictions(initializes: _data)
      init(initialValue) {
        _data = initialValue
      }

      get { _data }
      set { _data = newValue }
    }

    init() {
    }

    init(x: T, y: U) {
      self.data = (x, y)
    }
  }

  print("test-init-expr-2: \(TestPair<S, K>().data)")
  print("test-init-expr-2: \(TestPair<S, K>(x: S(x: 0), y: K(v: ["a", "b", "c"])).data)")

  struct TestAssign<T: P> {
    var _x: T
    var x: T = T.initialValue {
      @storageRestrictions(initializes: _x)
      init {
        _x = newValue
        print("TestAssign in x.init: self.x = \(_x)")
      }
      get { _x }
      set { _x = newValue }
    }
    var y: Int

    init(x1: T, x2: T, y: Int) {
      self.x = x1
      self.y = y
      self.x = x2
      print("TestAssign: self.x = \(self.x)")
    }
  }

  _ = TestAssign(x1: S(x: 0), x2: S(x: -3), y: 2)

  class TestDefault : CustomStringConvertible {
    var _a: Int

    var a: Int = 42 {
      @storageRestrictions(initializes: _a)
      init {
        _a = newValue
      }

      get { _a }
    }

    var b: String = "<<default>>"

    var description: String {
      "TestDefault(a: \(a), b: \(b))"
    }
  }

  print("test-init-expr-3: \(TestDefault())")
}

test_properties_with_inits()
// CHECK: test-init-expr-1: S(x: 42)
// CHECK-NEXT: test-init-expr-2: (S(x: 42), K(v: ["question"]))
// CHECK-NEXT: test-init-expr-2: (S(x: 0), K(v: ["a", "b", "c"]))
// CHECK-NEXT: TestAssign in x.init: self.x = S(x: 42)
// CHECK-NEXT: TestAssign in x.init: self.x = S(x: 0)
// CHECK-NEXT: TestAssign: self.x = S(x: -3)
// CHECK-NEXT: test-init-expr-3: TestDefault(a: 42, b: <<default>>)

func test_inheritance() {
  class Entity {
    var _age: Int = 0
    var age: Int = 0 {
      @storageRestrictions(initializes: _age)
      init { _age = newValue }
      get { _age }
      set { _age = newValue }
    }
  }

  class Person : Entity, CustomStringConvertible {
    var _firstName: String
    var firstName: String = "<<unknown>>" {
      @storageRestrictions(initializes: _firstName)
      init { _firstName = newValue }
      get { _firstName }
      set { _firstName = newValue }
    }

    var description: String {
      "Person(firstName: \(firstName), age: \(age))"
    }

    override init() {}

    init(firstName: String, age: Int) {
      super.init()
      self.firstName = firstName
      self.age = age
    }
  }

  print("test-inheritance-1: \(Person())")
  print("test-inheritance-2: \(Person(firstName: "Q", age: 42))")
}

test_inheritance()
// CHECK: test-inheritance-1: Person(firstName: <<unknown>>, age: 0)
// CHECK-NEXT: test-inheritance-2: Person(firstName: Q, age: 42)

do {
  class BackingData<T> {
    var data: [PartialKeyPath<T>: Any] = [:]

    func get<V>(_ key: KeyPath<T, V>) -> V { data[key] as! V }
    func set<V>(_ key: KeyPath<T, V>, _ value: V) {
      data[key] = value
    }
  }

  class Person : CustomStringConvertible {
    var description: String {
      "Person(name: \(name))"
    }

    private var backingData: BackingData<Person> = BackingData()

    private var _name: Int

    var name: String {
      @storageRestrictions(accesses: backingData, initializes: _name)
      init(newValue) {
        self.backingData.set(\.name, newValue)
        self._name = 0
      }

      get { self.backingData.get(\.name) }
      set { self.backingData.set(\.name, newValue) }
    }

    init(name: String) {
      self.name = name
    }

    init(backingData: BackingData<Person>) {
      self.backingData = backingData
      self._name = 0
    }
  }

  let person = Person(name: "P")
  print(person)

  let localData = BackingData<Person>()
  localData.set(\.name, "O")

  print(Person(backingData: localData))
}
// CHECK: Person(name: P)
// CHECK-NEXT: Person(name: O)

do {
  struct TestDefaultInitializable : CustomStringConvertible {
    var description: String {
      "TestDefaultInitializable(a: \(a))"
    }

    var _a: Int?
    var a: Int? {
      @storageRestrictions(initializes: _a)
      init { _a = newValue }
      get { _a }
    }
  }

  print(TestDefaultInitializable())
  print(TestDefaultInitializable(a: 42))

  struct TestMixedDefaultInitalizable : CustomStringConvertible {
    var description: String {
      "TestMixedDefaultInitalizable(a: \(a), b: \(b))"
    }

    var a: Int? {
      init {}
      get { nil }
    }

    var _b: String
    var b: String? {
      @storageRestrictions(initializes: _b)
      init { self._b = (newValue ?? "") }
      get { _b }
      set { _b = newValue ?? "" }
    }
  }

  print(TestMixedDefaultInitalizable())
  print(TestMixedDefaultInitalizable(b: "Hello"))
  print(TestMixedDefaultInitalizable(a: 42))
}
// CHECK: TestDefaultInitializable(a: nil)
// CHECK-NEXT: TestDefaultInitializable(a: Optional(42))
// CHECK-NEXT: TestMixedDefaultInitalizable(a: nil, b: Optional(""))
// CHECK-NEXT: TestMixedDefaultInitalizable(a: nil, b: Optional("Hello"))
// CHECK-NEXT: TestMixedDefaultInitalizable(a: nil, b: Optional(""))

do {
  struct TestNonMutatingSetDefault {
    var _count: Int

    var count: Int = 42 {
      @storageRestrictions(initializes: _count)
      init {
        _count = newValue
      }

      get { _count }
      nonmutating set {}
    }
  }

  struct TestNonMutatingSetNoDefault {
    var _count: Int
    var _other: String = ""

    var count: Int {
      @storageRestrictions(initializes: _count)
      init {
        print("init accessor is called: \(newValue)")
        _count = newValue
      }

      get { _count }

      nonmutating set {
        print("nonmutating set called: \(newValue)")
      }
    }

    init(value: Int) {
      self.count = value
      self.count = value + 1
    }
  }

  struct TestNonMutatingSetCustom {
    var _count: Int

    var count: Int = 42 {
      @storageRestrictions(initializes: _count)
      init {
        print("init accessor is called: \(newValue)")
        _count = newValue
      }

      get { _count }

      nonmutating set {
        print("nonmutating set called: \(newValue)")
      }
    }

    init(custom: Int) {
      count = custom
    }
  }

  print("test-nonmutating-set-1: \(TestNonMutatingSetDefault())")
  print("test-nonmutating-set-2: \(TestNonMutatingSetDefault(count: 0))")
  print("test-nonmutating-set-3: \(TestNonMutatingSetNoDefault(value: -1))")
  print("test-nonmutating-set-4: \(TestNonMutatingSetCustom(custom: 0))")
}
// CHECK: test-nonmutating-set-1: TestNonMutatingSetDefault(_count: 42)
// CHECK-NEXT: test-nonmutating-set-2: TestNonMutatingSetDefault(_count: 0)
// CHECK-NEXT: init accessor is called: -1
// CHECK-NEXT: nonmutating set called: 0
// CHECK-NEXT: test-nonmutating-set-3: TestNonMutatingSetNoDefault(_count: -1, _other: "")
// CHECK-NEXT: init accessor is called: 42
// CHECK-NEXT: nonmutating set called: 0
// CHECK-NEXT: test-nonmutating-set-4: TestNonMutatingSetCustom(_count: 42)

do {
  class Base {
    var _count: Int

    var count: Int {
      @storageRestrictions(initializes: _count)
      init {
        print("init accessor with Self = \(Self.self)")
        _count = newValue
      }

      get { _count }

      set {}
    }

    init() {
      count = 42
    }
  }

  class Sub: Base {}

  print("- init accessor vs dynamic Self")
  _ = Base()
  _ = Sub()
}

// CHECK-NEXT: - init accessor vs dynamic Self
// CHECK-NEXT: init accessor with Self = Base
// CHECK-NEXT: init accessor with Self = Sub