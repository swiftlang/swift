// RUN: %empty-directory(%t)
// RUN: %target-build-swift -enable-experimental-feature TypeWrappers -parse-as-library -emit-library -emit-module-path %t/type_wrapper_defs.swiftmodule -module-name type_wrapper_defs %S/Inputs/type_wrapper_defs.swift -o %t/%target-library-name(type_wrapper_defs)
// RUN: %target-build-swift -ltype_wrapper_defs -module-name main -I %t -L %t %s -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(type_wrapper_defs) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: asserts

// This requires executable tests to be run on the same machine as the compiler,
// as it links with a dylib that it doesn't arrange to get uploaded to remote executors.
// (rdar://99051588)
// UNSUPPORTED: remote_run || device_run

import type_wrapper_defs

var p: Person<String> = .init(name: "P", projects: ["A", "B"])
// CHECK: Wrapper.init(for: Person<String>, storage: $Storage(name: "P", projects: ["A", "B"]))

print(p.name)
// CHECK: in (reference type) getter storage: \$Storage.name
// CHECK-NEXT: P
print(p.projects)
// CHECK: in (reference type) getter storage: \$Storage.projects
// CHECK-NEXT: ["A", "B"]

p.name = "OtherP"
// CHECK: in (reference type) setter => OtherP
p.projects.append("C")
// CHECK: in (reference type) getter storage: \$Storage.projects
// CHECK-NEXT: in (reference type) setter => ["A", "B", "C"]


func addProjects<T>(p: inout Person<T>, _ newProjects: [T]) {
  p.projects.append(contentsOf: newProjects)
}

addProjects(p: &p, ["D"])
// CHECK: in (reference type) getter storage: \$Storage.projects
// CHECK: in (reference type) setter => ["A", "B", "C", "D"]

print(p.name)
// CHECK: in (reference type) getter storage: \$Storage.name
// CHECK-NEXT: OtherP

print(p.projects)
// CHECK: in (reference type) getter storage: \$Storage.projects
// CHECK-NEXT: ["A", "B", "C", "D"]

var pDefaults = PersonWithDefaults()
// CHECK: Wrapper.init(for: PersonWithDefaults, storage: $Storage(name: "<no name>", age: 99))

print(pDefaults.name)
// CHECK: in getter storage: \$Storage.name
// CHECK: <no name>

print(pDefaults.age)
// CHECK: in getter storage: \$Storage.age
// CHECK: 99

pDefaults.name = "Actual Name"
// CHECK-NEXT: in setter => Actual Name

pDefaults.age = 0
// CHECK-NEXT: in setter => 0

print(pDefaults.name)
// CHECK: in getter storage: \$Storage.name
// CHECK: Actual Name

print(pDefaults.age)
// CHECK: in getter storage: \$Storage.age
// CHECK: 0

let pDefaultsAge = PersonWithDefaults(name: "Actual Name")

print(pDefaultsAge.name)
// CHECK: in getter storage: \$Storage.name
// CHECK: Actual Name

print(pDefaultsAge.age)
// CHECK: in getter storage: \$Storage.age
// CHECK: 99

let pDefaultsName = PersonWithDefaults(age: 31337)

print(pDefaultsName.name)
// CHECK: in getter storage: \$Storage.name
// CHECK: <no name>

print(pDefaultsName.age)
// CHECK: in getter storage: \$Storage.age
// CHECK: 31337

func testPropertyWrappers() {
  var wrapped1 = PropWrapperTest(test: 42)
  // CHECK: Wrapper.init(for: PropWrapperTest, storage: $Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 42)))
  do {
    print(wrapped1.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 42

    wrapped1.test = 0
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 0)

    print(wrapped1.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 0
  }

  var wrapped2 = DefaultedPropWrapperTest()
  // CHECK: Wrapper.init(for: DefaultedPropWrapperTest, storage: $Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 0)))
  do {
    print(wrapped2.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 0

    wrapped2.test = 42
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 42)

    print(wrapped2.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 42
  }

  var wrapped3 = DefaultedPropWrapperTest(test: 1)
  // CHECK: Wrapper.init(for: DefaultedPropWrapperTest, storage: $Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 1)))
  do {
    print(wrapped3.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 1

    wrapped3.test = 31337
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 31337)

    print(wrapped3.test)
    // CHECK: in getter
    // CHECK-NEXT: 31337
  }

  var wrapped4 = DefaultedPropWrapperWithArgTest()
  // CHECK: Wrapper.init(for: DefaultedPropWrapperWithArgTest, storage: $Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 3)))
  do {
    print(wrapped4.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 3

    wrapped4.test = 0
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 0)

    print(wrapped4.test)
    // CHECK: in getter storage: \$Storage._test
    // CHECK-NEXT: 0
  }

  var wrapped5 = PropWrapperNoInitTest(a: PropWrapperWithoutInit(value: 1))
  // CHECK: Wrapper.init(for: PropWrapperNoInitTest, storage: $Storage(_a: type_wrapper_defs.PropWrapperWithoutInit<Swift.Int>(value: 1), _b: type_wrapper_defs.PropWrapperWithoutInit<Swift.String>(value: "b")))
  do {
    print(wrapped5.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: 1

    print(wrapped5.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: b

    wrapped5.a = 42
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<Int>(value: 42)

    wrapped5.b = "not b"
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<String>(value: "not b")

    print(wrapped5.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: 42

    print(wrapped5.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: not b
  }

  var wrapped6 = PropWrapperNoInitTest(a: PropWrapperWithoutInit(value: 1), b: PropWrapperWithoutInit(value: "hello"))
  // CHECK: Wrapper.init(for: PropWrapperNoInitTest, storage: $Storage(_a: type_wrapper_defs.PropWrapperWithoutInit<Swift.Int>(value: 1), _b: type_wrapper_defs.PropWrapperWithoutInit<Swift.String>(value: "hello")))
  do {
    print(wrapped6.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: 1

    print(wrapped6.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: hello

    wrapped6.a = 42
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<Int>(value: 42)

    wrapped6.b = "b"
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<String>(value: "b")

    print(wrapped6.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: 42

    print(wrapped6.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: b
  }

  var wrapped7 = ComplexPropWrapperTest()
  // CHECK: Wrapper.init(for: ComplexPropWrapperTest, storage: $Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: ["a"]), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))))
  do {
    print(wrapped7.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a"]

    print(wrapped7.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [1, 2, 3]

    wrapped7.a = ["a", "b", "c"]
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped7.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped7.b = [0]
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))

    print(wrapped7.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [0]
  }

  var wrapped8 = ComplexPropWrapperTest(a: ["a", "b"])
  // CHECK: Wrapper.init(for: ComplexPropWrapperTest, storage: $Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: ["a", "b"]), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))))
  do {
    print(wrapped8.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a", "b"]

    print(wrapped8.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [1, 2, 3]

    wrapped8.a = ["a", "b", "c"]
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped8.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped8.b = [0]
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))

    print(wrapped8.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [0]
  }

  var wrapped9 = ComplexPropWrapperTest(b: PropWrapperWithoutInit(value: PropWrapper(wrappedValue: [0])))
  // CHECK: Wrapper.init(for: ComplexPropWrapperTest, storage: $Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: ["a"]), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))))
  do {
    print(wrapped9.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a"]

    print(wrapped9.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [0]

    wrapped9.a = ["a", "b", "c"]
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped9.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped9.b = [1, 2, 3]
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))

    print(wrapped9.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [1, 2, 3]
  }

  var wrapped10 = ComplexPropWrapperTest(a: [], b: PropWrapperWithoutInit(value: PropWrapper(wrappedValue: [0])))
  // CHECK: Wrapper.init(for: ComplexPropWrapperTest, storage: $Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: []), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))))
  do {
    print(wrapped10.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: []

    print(wrapped10.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [0]

    wrapped10.a = ["a", "b", "c"]
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped10.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped10.b = [1, 2, 3]
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))

    print(wrapped10.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: [1, 2, 3]
  }

  var wrapped11 = PropWrapperNoProjectionTest()
  // CHECK: Wrapper.init(for: PropWrapperNoProjectionTest, storage: $Storage(_a: type_wrapper_defs.PropWrapperWithoutProjection<Swift.Int>(value: 0), _b: type_wrapper_defs.PropWrapperWithoutProjection<type_wrapper_defs.PropWrapper<Swift.String>>(value: type_wrapper_defs.PropWrapper<Swift.String>(value: "b"))))
  do {
    print(wrapped11.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: 0

    print(wrapped11.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: b

    wrapped11.a = 42
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: in setter => PropWrapperWithoutProjection<Int>(value: 42)

    wrapped11.b = "not b"
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: in setter => PropWrapperWithoutProjection<PropWrapper<String>>(value: type_wrapper_defs.PropWrapper<Swift.String>(value: "not b"))

    print(wrapped11.a)
    // CHECK: in getter storage: \$Storage._a
    // CHECK-NEXT: 42

    print(wrapped11.b)
    // CHECK: in getter storage: \$Storage._b
    // CHECK-NEXT: not b
  }
}

testPropertyWrappers()

do {
  var person = PersonWithUnmanagedTest(name: "Arthur Dent")
  // CHECK: Wrapper.init(for: PersonWithUnmanagedTest, storage: $Storage(name: "Arthur Dent", _favoredColor: type_wrapper_defs.PropWrapper<Swift.String>(value: "red")))

  print(person.name)
  // CHECK: in read-only getter storage: \$Storage.name
  // CHECK-NEXT: Arthur Dent

  print(person.age)
  // CHECK: 30

  print(person.placeOfBirth)
  // CHECK: Earth

  print(person.favoredColor)
  // CHECK: in getter storage: \$Storage._favoredColor
  // CHECK-NEXT: red

  person.favoredColor = "yellow"
  // CHECK: in getter storage: \$Storage._favoredColor
  // CHECK-NEXT: in setter => PropWrapper<String>(value: "yellow")

  print(person.favoredColor)
  // CHECK: in getter storage: \$Storage._favoredColor
  // CHECK-NEXT: yellow
}

do {
  var test = ClassWithDesignatedInit(a: 42)

  print(test.a)
  // CHECK: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: 42

  print(test.b)
  // CHECK: in (reference type) getter storage: \$Storage._b
  // CHECK-NEXT: [1, 2, 3]

  test.a = 0
  // CHECK: in (reference type) setter => 0

  test.b = [42]
  // CHECK: in (reference type) getter storage: \$Storage._b
  // CHECK-NEXT: in (reference type) setter => PropWrapperWithoutInit<Array<Int>>(value: [42])

  print(test.a)
  // CHECK: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: 0

  print(test.b)
  // CHECK: in (reference type) getter storage: \$Storage._b
  // CHECK-NEXT: [42]
}

do {
  var arthur = PersonWithIgnoredAge(name: "Arthur Dent", age: 30)
  // CHECK: Wrapper.init(for: PersonWithIgnoredAge, storage: $Storage(name: "Arthur Dent"))

  print(arthur.name)
  // CHECK: in (reference type) getter storage: \$Storage.name
  // CHECK-NEXT: Arthur Dent

  print(arthur.age)
  // CHECK-NOT: in getter
  // CHECK-NEXT: 30

  print(arthur.manufacturer)
  // CHECK-NOT: in getter
  // CHECK-NEXT: nil

  arthur.age = 32
  // CHECK-NOT: in setter

  var marvin = PersonWithIgnoredAge(name: "Marvin The Paranoid Android", manufacturer: "Sirius Cybernetics Corporation")
  // CHECK: Wrapper.init(for: PersonWithIgnoredAge, storage: $Storage(name: "Marvin The Paranoid Android"))

  print(marvin.name)
  // CHECK: in (reference type) getter storage: \$Storage.name
  // CHECK-NEXT: Marvin The Paranoid Android

  print(marvin.age)
  // CHECK-NOT: in getter
  // CHECK-NEXT: 0

  print(marvin.manufacturer)
  // CHECK-NOT: in getter
  // CHECK-NEXT: Sirius Cybernetics Corporation

  marvin.age = 1000
  // CHECK-NOT: in setter

  marvin.manufacturer = nil
  // CHECK-NOT: in setter
}

// user-defined init tests
do {
  _ = EmptyUserDefinedInitClassTest()
  // CHECK: Wrapper.init(for: EmptyUserDefinedInitClassTest, storage: $Storage())
  _ = EmptyUserDefinedInitStructTest()
  // CHECK: Wrapper.init(for: EmptyUserDefinedInitStructTest, storage: $Storage())

  _ = TrivialUserDefinedInitClassTest(a: 42)
  // CHECK: Wrapper.init(for: TrivialUserDefinedInitClassTest, storage: $Storage(a: 42))

  _ = TrivialUserDefinedInitClassTest(withReassign: 42)
  // CHECK: Wrapper.init(for: TrivialUserDefinedInitClassTest, storage: $Storage(a: 0))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: 0
  // CHECK-NEXT: in (reference type) setter => 42
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: 42

  _ = TrivialUserDefinedInitStructTest(withReassign: 42)
  // CHECK: Wrapper.init(for: TrivialUserDefinedInitStructTest, storage: $Storage(a: 0))
  // CHECK-NEXT: in getter storage: \$Storage.a
  // CHECK-NEXT: 0
  // CHECK-NEXT: in setter => 42
  // CHECK-NEXT: in getter storage: \$Storage.a
  // CHECK-NEXT: 42

  let complex1 = ContextUserDefinedInitClassTest(c: ["hello": 42], placeholder: ("<placeholder>", -1))
  // CHECK: Wrapper.init(for: ContextUserDefinedInitClassTest<String, Int>, storage: $Storage(a: 0, _b: type_wrapper_defs.PropWrapper<(Swift.String, (Swift.Int, Swift.Array<Swift.Int>))>(value: ("", (0, [1, 2, 3]))), c: ["hello": 42]))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
  // CHECK-NEXT: ["hello": 42]
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
  // CHECK-NEXT: in (reference type) setter => [{{.*}}, {{.*}}]
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
  // CHECK-NEXT: [{{.*}}, {{.*}}]
  print(complex1.a)
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: 0
  print(complex1.b)
  // CHECK-NEXT: in (reference type) getter storage: \$Storage._b
  // CHECK-NEXT: ("", (0, [1, 2, 3]))

  if complex1.c == ["hello": 42] { // use of Hashable, Equatable conformances
    // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
    fatalError("== failed between complex1 dictionaries")
  }

  if complex1.c != ["hello": 42, "<placeholder>": -1] {
    // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
    fatalError("!= failed between complex1 dictionaries")
  }

  let complex2 = ContextUserDefinedInitStructTest(b: ("", (0, [1])), c: ["hello": 42], placeholder: ("<placeholder>", -1))
  // CHECK: Wrapper.init(for: ContextUserDefinedInitStructTest<String, Int>, storage: $Storage(a: 0, _b: type_wrapper_defs.PropWrapper<(Swift.String, (Swift.Int, Swift.Array<Swift.Int>))>(value: ("", (0, [1]))), c: ["hello": 42]))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
  // CHECK-NEXT: ["hello": 42]
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
  // CHECK-NEXT: in (reference type) setter => [{{.*}}, {{.*}}]
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
  // CHECK-NEXT: [{{.*}}, {{.*}}]
  print(complex2.a)
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: 0
  print(complex2.b)
  // CHECK-NEXT: in (reference type) getter storage: \$Storage._b
  // CHECK-NEXT: ("", (0, [1]))
  if complex2.c == ["hello": 42] { // use of Hashable, Equatable conformances
    // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
    fatalError("== failed between complex2 dictionaries")
  }

  if complex2.c != ["hello": 42, "<placeholder>": -1] {
    // CHECK-NEXT: in (reference type) getter storage: \$Storage.c
    fatalError("!= failed between complex2 dictionaries")
  }

  // cond: true, initialValue: nil
  _ = UserDefinedInitWithConditionalTest<Int>()
  // CHECK: Wrapper.init(for: UserDefinedInitWithConditionalTest<Int>, storage: $Storage(val: nil))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.val
  // CHECK-NEXT nil

  // initalValue: nil
  _ = UserDefinedInitWithConditionalTest<[String: any BinaryInteger]>(cond: true)
  // CHECK: Wrapper.init(for: UserDefinedInitWithConditionalTest<Dictionary<String, BinaryInteger>>, storage: $Storage(val: nil))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.val
  // CHECK-NEXT: nil

  do {
    let initialValue = (("a", 42), ("b", 0))

    _ = UserDefinedInitWithConditionalTest(cond: true, initialValue: initialValue)
    // CHECK: Wrapper.init(for: UserDefinedInitWithConditionalTest<((String, Int), (String, Int))>, storage: $Storage(val: Optional((("a", 42), ("b", 0)))))
    // CHECK-NEXT: in (reference type) getter storage: \$Storage.val
    // CHECK-NEXT: Optional((("a", 42), ("b", 0)))

    _ = UserDefinedInitWithConditionalTest(cond: false, initialValue: initialValue)
    // CHECK: Wrapper.init(for: UserDefinedInitWithConditionalTest<((String, Int), (String, Int))>, storage: $Storage(val: nil))
    // CHECK-NEXT: in (reference type) getter storage: \$Storage.val
    // CHECK-NEXT: nil
  }
}

do {
  let test1 = ClassWithConvenienceInit(val: [1, 2, 3])
  // CHECK: Wrapper.init(for: ClassWithConvenienceInit<Array<Int>>, storage: $Storage(a: Optional([1, 2, 3]), b: "<default-placeholder>"))
  // CHECK: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: [1, 2, 3]
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.b
  // CHECK-NEXT: <placeholder>
  // CHECK-NEXT: in (reference type) setter => <modified>
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.b
  // CHECK-NEXT: <modified>

  let test2 = ClassWithConvenienceInit(aWithoutB: [1, ""])
  // CHECK: Wrapper.init(for: ClassWithConvenienceInit<Array<Any>>, storage: $Storage(a: Optional([1, ""]), b: "<default-placeholder>"))

  func test<T>(_ v: T) {
    let test1 = ClassWithConvenienceInit<(Int, String, T)>()
    test1.a = (-1, "ultimate question", v)
    print(test1.a)
  }

  test((a: 1, b: 2.0, c: 3))
  // CHECK: Wrapper.init(for: ClassWithConvenienceInit<(Int, String, (a: Int, b: Double, c: Int))>, storage: $Storage(a: nil, b: "<default-placeholder>"))
  // -> from init(a: T?)
  // CHECK: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: nil
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.b
  // CHECK-NEXT: <placeholder>
  // CHECK-NEXT: in (reference type) setter => <modified>
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.b
  // CHECK-NEXT: <modified>
  // -> from init()
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: nil
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.b
  // CHECK-NEXT: <modified>
  // -> from test<T>(_: T)
  // CHECK-NEXT: in (reference type) setter => Optional((-1, "ultimate question", (a: 1, b: 2.0, c: 3)))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage.a
  // CHECK-NEXT: Optional((-1, "ultimate question", (a: 1, b: 2.0, c: 3)))
}

do {
  class X : CustomStringConvertible {
    var x: [Int] = []

    var description: String {
      "X(x: \(x))"
    }
  }

  var arg = X()

  let test1 = TypeWithLetProperties(a: arg, b: 42) {
    arg.x.append(1)
  }
  // CHECK: Wrapper.init(for: TypeWithLetProperties<X>, storage: $Storage(a: X(x: []), b: 42))
  // CHECK-NEXT: --Before onSet--
  // CHECK-NEXT: in read-only getter storage: \$Storage.a
  // CHECK-NEXT: X(x: [])
  // CHECK-NEXT: in read-only getter storage: \$Storage.b
  // CHECK-NEXT: 42
  // CHECK-NEXT: --After onSet--
  // CHECK-NEXT: in read-only getter storage: \$Storage.a
  // CHECK-NEXT: X(x: [1])
  // CHECK-NEXT: in read-only getter storage: \$Storage.b
  // CHECK-nEXT: 42

  let test2 = TypeWithLetProperties(a: Optional.some([1, 2, 3]))
  // CHECK: Wrapper.init(for: TypeWithLetProperties<Optional<Array<Int>>>, storage: $Storage(a: Optional([1, 2, 3]), b: 0))
  // CHECK-NEXT: --Before onSet--
  // CHECK-NEXT: in read-only getter storage: \$Storage.a
  // CHECK-NEXT: Optional([1, 2, 3])
  // CHECK-NEXT: in read-only getter storage: \$Storage.b
  // CHECK-NEXT: 0

  let test3 = TypeWithDefaultedLetProperties<[String]>()
  // CHECK: Wrapper.init(for: TypeWithDefaultedLetProperties<Array<String>>, storage: $Storage(a: nil, b: 0))
  // CHECK-NEXT: in (reference type) let getter storage: \$Storage.a
  // CHECK-NEXT: nil
  // CHECK-NEXT: in (reference type) let getter storage: \$Storage.b
  // CHECK-NEXT: 0

  let test4 = TypeWithSomeDefaultedLetProperties(a: ["", 1.0])
  // CHECK: Wrapper.init(for: TypeWithSomeDefaultedLetProperties<Array<Any>>, storage: $Storage(a: ["", 1.0], b: Optional(0), _c: type_wrapper_defs.PropWrapper<Swift.String>(value: "<default>"), _d: type_wrapper_defs.PropWrapperWithoutInit<Swift.Array<Any>>(value: [1, ""])))
  // CHECK-NEXT: in (reference type) getter storage: \$Storage._c
  // CHECK-NEXT: in (reference type) setter => PropWrapper<String>(value: "a")
  // CHECK-NEXT: in (reference type) let getter storage: \$Storage.a
  // CHECK-NEXT: ["", 1.0]
  // CHECK-NEXT: in (reference type) let getter storage: \$Storage.b
  // CHECK-NEXT: Optional(0)
  // CHECK-NEXT: in (reference type) getter storage: \$Storage._c
  // CHECK-NEXT: a
}

do {
  @Wrapper
  struct Test<T> {
    var a: T
    var b: Int
  }

  let wrapper = Wrapper(for: Test<[Float?]>.self, storage: Test<[Float?]>.$Storage(a: [1.0], b: 42))
  // CHECK: Wrapper.init(for: Test<Array<Optional<Float>>>, storage: $Storage(a: [Optional(1.0)], b: 42))

  var test = Test(storageWrapper: wrapper)

  print(test.a)
  print(test.b)
  // CHECK-NEXT: in getter storage: \$Storage.a
  // CHECK-NEXT: [Optional(1.0)]
  // CHECK-NEXT: in getter storage: \$Storage.b
  // CHECK-NEXT: 42
}

// Type wrapper inference from protocols
do {
  struct Test<T> : WrappedProtocol {
    var v: [T?]

    init(_ value: T? = nil) {
      v = [value]
    }
  }

  func run_test<T: WrappedProtocol>(_ test: T) {
    print(test.v)
    print(test.v.count)
  }

  run_test(Test(Optional("Hello")))
  // CHECK: Wrapper.init(for: Test<String>, storage: $Storage(v: [Optional("Hello")]))
  // CHECK-NEXT: in getter storage: \$Storage.v
  // CHECK-NEXT: [Optional("Hello")]
  // CHECK-NEXT: in getter storage: \$Storage.v
  // CHECK-NEXT: 1
}
