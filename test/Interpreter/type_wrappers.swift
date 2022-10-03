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
// CHECK: Wrapper.init($Storage(name: "P", projects: ["A", "B"]))

print(p.name)
// CHECK: in getter
// CHECK-NEXT: P
print(p.projects)
// CHECK: in getter
// CHECK-NEXT: ["A", "B"]

p.name = "OtherP"
// CHECK: in setter => OtherP
p.projects.append("C")
// CHECK: in getter
// CHECK-NEXT: in setter => ["A", "B", "C"]


func addProjects<T>(p: inout Person<T>, _ newProjects: [T]) {
  p.projects.append(contentsOf: newProjects)
}

addProjects(p: &p, ["D"])
// CHECK: in getter
// CHECK: in setter => ["A", "B", "C", "D"]

print(p.name)
// CHECK: in getter
// CHECK-NEXT: OtherP

print(p.projects)
// CHECK: in getter
// CHECK-NEXT: ["A", "B", "C", "D"]

var pDefaults = PersonWithDefaults()
// CHECK: Wrapper.init($Storage(name: "<no name>", age: 99))

print(pDefaults.name)
// CHECK: in getter
// CHECK: <no name>

print(pDefaults.age)
// CHECK: in getter
// CHECK: 99

pDefaults.name = "Actual Name"
// CHECK-NEXT: in setter => Actual Name

pDefaults.age = 0
// CHECK-NEXT: in setter => 0

print(pDefaults.name)
// CHECK: in getter
// CHECK: Actual Name

print(pDefaults.age)
// CHECK: in getter
// CHECK: 0

let pDefaultsAge = PersonWithDefaults(name: "Actual Name")

print(pDefaultsAge.name)
// CHECK: in getter
// CHECK: Actual Name

print(pDefaultsAge.age)
// CHECK: in getter
// CHECK: 99

let pDefaultsName = PersonWithDefaults(age: 31337)

print(pDefaultsName.name)
// CHECK: in getter
// CHECK: <no name>

print(pDefaultsName.age)
// CHECK: in getter
// CHECK: 31337

func testPropertyWrappers() {
  var wrapped1 = PropWrapperTest(test: 42)
  // CHECK: Wrapper.init($Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 42)))
  do {
    print(wrapped1.test)
    // CHECK: in getter
    // CHECK-NEXT: 42

    wrapped1.test = 0
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 0)

    print(wrapped1.test)
    // CHECK: in getter
    // CHECK-NEXT: 0
  }

  var wrapped2 = DefaultedPropWrapperTest()
  // CHECK: Wrapper.init($Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 0)))
  do {
    print(wrapped2.test)
    // CHECK: in getter
    // CHECK-NEXT: 0

    wrapped2.test = 42
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 42)

    print(wrapped2.test)
    // CHECK: in getter
    // CHECK-NEXT: 42
  }

  var wrapped3 = DefaultedPropWrapperTest(test: 1)
  // CHECK: Wrapper.init($Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 1)))
  do {
    print(wrapped3.test)
    // CHECK: in getter
    // CHECK-NEXT: 1

    wrapped3.test = 31337
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 31337)

    print(wrapped3.test)
    // CHECK: in getter
    // CHECK-NEXT: 31337
  }

  var wrapped4 = DefaultedPropWrapperWithArgTest()
  // CHECK: Wrapper.init($Storage(_test: type_wrapper_defs.PropWrapper<Swift.Int>(value: 3)))
  do {
    print(wrapped4.test)
    // CHECK: in getter
    // CHECK-NEXT: 3

    wrapped4.test = 0
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Int>(value: 0)

    print(wrapped4.test)
    // CHECK: in getter
    // CHECK-NEXT: 0
  }

  var wrapped5 = PropWrapperNoInitTest(a: PropWrapperWithoutInit(value: 1))
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapperWithoutInit<Swift.Int>(value: 1), _b: type_wrapper_defs.PropWrapperWithoutInit<Swift.String>(value: "b")))
  do {
    print(wrapped5.a)
    // CHECK: in getter
    // CHECK-NEXT: 1

    print(wrapped5.b)
    // CHECK: in getter
    // CHECK-NEXT: b

    wrapped5.a = 42
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<Int>(value: 42)

    wrapped5.b = "not b"
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<String>(value: "not b")

    print(wrapped5.a)
    // CHECK: in getter
    // CHECK-NEXT: 42

    print(wrapped5.b)
    // CHECK: in getter
    // CHECK-NEXT: not b
  }

  var wrapped6 = PropWrapperNoInitTest(a: PropWrapperWithoutInit(value: 1), b: PropWrapperWithoutInit(value: "hello"))
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapperWithoutInit<Swift.Int>(value: 1), _b: type_wrapper_defs.PropWrapperWithoutInit<Swift.String>(value: "hello")))
  do {
    print(wrapped6.a)
    // CHECK: in getter
    // CHECK-NEXT: 1

    print(wrapped6.b)
    // CHECK: in getter
    // CHECK-NEXT: hello

    wrapped6.a = 42
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<Int>(value: 42)

    wrapped6.b = "b"
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<String>(value: "b")

    print(wrapped6.a)
    // CHECK: in getter
    // CHECK-NEXT: 42

    print(wrapped6.b)
    // CHECK: in getter
    // CHECK-NEXT: b
  }

  var wrapped7 = ComplexPropWrapperTest()
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: ["a"]), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))))
  do {
    print(wrapped7.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a"]

    print(wrapped7.b)
    // CHECK: in getter
    // CHECK-NEXT: [1, 2, 3]

    wrapped7.a = ["a", "b", "c"]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped7.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped7.b = [0]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))

    print(wrapped7.b)
    // CHECK: in getter
    // CHECK-NEXT: [0]
  }

  var wrapped8 = ComplexPropWrapperTest(a: ["a", "b"])
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: ["a", "b"]), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))))
  do {
    print(wrapped8.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a", "b"]

    print(wrapped8.b)
    // CHECK: in getter
    // CHECK-NEXT: [1, 2, 3]

    wrapped8.a = ["a", "b", "c"]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped8.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped8.b = [0]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))

    print(wrapped8.b)
    // CHECK: in getter
    // CHECK-NEXT: [0]
  }

  var wrapped9 = ComplexPropWrapperTest(b: PropWrapperWithoutInit(value: PropWrapper(wrappedValue: [0])))
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: ["a"]), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))))
  do {
    print(wrapped9.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a"]

    print(wrapped9.b)
    // CHECK: in getter
    // CHECK-NEXT: [0]

    wrapped9.a = ["a", "b", "c"]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped9.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped9.b = [1, 2, 3]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))

    print(wrapped9.b)
    // CHECK: in getter
    // CHECK-NEXT: [1, 2, 3]
  }

  var wrapped10 = ComplexPropWrapperTest(a: [], b: PropWrapperWithoutInit(value: PropWrapper(wrappedValue: [0])))
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapper<Swift.Array<Swift.String>>(value: []), _b: type_wrapper_defs.PropWrapperWithoutInit<type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [0]))))
  do {
    print(wrapped10.a)
    // CHECK: in getter
    // CHECK-NEXT: []

    print(wrapped10.b)
    // CHECK: in getter
    // CHECK-NEXT: [0]

    wrapped10.a = ["a", "b", "c"]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapper<Array<String>>(value: ["a", "b", "c"])

    print(wrapped10.a)
    // CHECK: in getter
    // CHECK-NEXT: ["a", "b", "c"]

    wrapped10.b = [1, 2, 3]
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutInit<PropWrapper<Array<Int>>>(value: type_wrapper_defs.PropWrapper<Swift.Array<Swift.Int>>(value: [1, 2, 3]))

    print(wrapped10.b)
    // CHECK: in getter
    // CHECK-NEXT: [1, 2, 3]
  }

  var wrapped11 = PropWrapperNoProjectionTest()
  // CHECK: Wrapper.init($Storage(_a: type_wrapper_defs.PropWrapperWithoutProjection<Swift.Int>(value: 0), _b: type_wrapper_defs.PropWrapperWithoutProjection<type_wrapper_defs.PropWrapper<Swift.String>>(value: type_wrapper_defs.PropWrapper<Swift.String>(value: "b"))))
  do {
    print(wrapped11.a)
    // CHECK: in getter
    // CHECK-NEXT: 0

    print(wrapped11.b)
    // CHECK: in getter
    // CHECK-NEXT: b

    wrapped11.a = 42
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutProjection<Int>(value: 42)

    wrapped11.b = "not b"
    // CHECK: in getter
    // CHECK-NEXT: in setter => PropWrapperWithoutProjection<PropWrapper<String>>(value: type_wrapper_defs.PropWrapper<Swift.String>(value: "not b"))

    print(wrapped11.a)
    // CHECK: in getter
    // CHECK-NEXT: 42

    print(wrapped11.b)
    // CHECK: in getter
    // CHECK-NEXT: not b
  }
}

testPropertyWrappers()

do {
  var person = PersonWithUnmanagedTest(name: "Arthur Dent")
  // CHECK: Wrapper.init($Storage(_favoredColor: type_wrapper_defs.PropWrapper<Swift.String>(value: "red")))

  print(person.name)
  // CHECK: Arthur Dent

  print(person.age)
  // CHECK: 30

  print(person.placeOfBirth)
  // CHECK: Earth

  print(person.favoredColor)
  // CHECK: in getter
  // CHECK-NEXT: red

  person.favoredColor = "yellow"
  // CHECK: in getter
  // CHECK-NEXT: in setter => PropWrapper<String>(value: "yellow")

  print(person.favoredColor)
  // CHECK: in getter
  // CHECK-NEXT: yellow
}

do {
  var test = ClassWithDesignatedInit(a: 42)

  print(test.a)
  // CHECK: in getter
  // CHECK-NEXT: 42

  print(test.b)
  // CHECK: in getter
  // CHECK-NEXT: [1, 2, 3]

  test.a = 0
  // CHECK: in setter => 0

  test.b = [42]
  // CHECK: in getter
  // CHECK-NEXT: in setter => PropWrapperWithoutInit<Array<Int>>(value: [42])

  print(test.a)
  // CHECK: in getter
  // CHECK-NEXT: 0

  print(test.b)
  // CHECK: in getter
  // CHECK-NEXT: [42]
}

do {
  var arthur = PersonWithIgnoredAge(name: "Arthur Dent", age: 30)
  // CHECK: Wrapper.init($Storage(name: "Arthur Dent"))

  print(arthur.name)
  // CHECK: in getter
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
  // CHECK: Wrapper.init($Storage(name: "Marvin The Paranoid Android"))

  print(marvin.name)
  // CHECK: in getter
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
  // CHECK: Wrapper.init($Storage())
  _ = EmptyUserDefinedInitStructTest()
  // CHECK: Wrapper.init($Storage())

  _ = TrivialUserDefinedInitClassTest(a: 42)
  // CHECK: Wrapper.init($Storage(a: 42))

  _ = TrivialUserDefinedInitClassTest(withReassign: 42)
  // CHECK: Wrapper.init($Storage(a: 0))
  // CHECK-NEXT: in getter
  // CHECK-NEXT: 0
  // CHECK-NEXT: in setter => 42
  // CHECK-NEXT: in getter
  // CHECK-NEXT: 42

  _ = TrivialUserDefinedInitStructTest(withReassign: 42)
  // CHECK: Wrapper.init($Storage(a: 0))
  // CHECK-NEXT: in getter
  // CHECK-NEXT: 0
  // CHECK-NEXT: in setter => 42
  // CHECK-NEXT: in getter
  // CHECK-NEXT: 42

  let complex1 = ContextUserDefinedInitClassTest(c: ["hello": 42], placeholder: ("<placeholder>", -1))
  // CHECK: Wrapper.init($Storage(a: 0, _b: type_wrapper_defs.PropWrapper<(Swift.String, (Swift.Int, Swift.Array<Swift.Int>))>(value: ("", (0, [1, 2, 3]))), c: ["hello": 42]))
  // CHECK-NEXT: in getter
  // CHECK-NEXT: ["hello": 42]
  // CHECK-NEXT: in getter
  // CHECK-NEXT: in setter => [{{.*}}, {{.*}}]
  // CHECK-NEXT: in getter
  // CHECK-NEXT: [{{.*}}, {{.*}}]
  print(complex1.a)
  // CHECK-NEXT: in getter
  // CHECK-NEXT: 0
  print(complex1.b)
  // CHECK-NEXT: in getter
  // CHECK-NEXT: ("", (0, [1, 2, 3]))

  if complex1.c == ["hello": 42] { // use of Hashable, Equatable conformances
    // CHECK-NEXT: in getter
    fatalError("== failed between complex1 dictionaries")
  }

  if complex1.c != ["hello": 42, "<placeholder>": -1] {
    // CHECK-NEXT: in getter
    fatalError("!= failed between complex1 dictionaries")
  }

  let complex2 = ContextUserDefinedInitStructTest(b: ("", (0, [1])), c: ["hello": 42], placeholder: ("<placeholder>", -1))
  // CHECK: Wrapper.init($Storage(a: 0, _b: type_wrapper_defs.PropWrapper<(Swift.String, (Swift.Int, Swift.Array<Swift.Int>))>(value: ("", (0, [1]))), c: ["hello": 42]))
    // CHECK-NEXT: in getter
  // CHECK-NEXT: ["hello": 42]
  // CHECK-NEXT: in getter
  // CHECK-NEXT: in setter => [{{.*}}, {{.*}}]
  // CHECK-NEXT: in getter
  // CHECK-NEXT: [{{.*}}, {{.*}}]
  print(complex2.a)
  // CHECK-NEXT: in getter
  // CHECK-NEXT: 0
  print(complex2.b)
  // CHECK-NEXT: in getter
  // CHECK-NEXT: ("", (0, [1]))
  if complex2.c == ["hello": 42] { // use of Hashable, Equatable conformances
    // CHECK-NEXT: in getter
    fatalError("== failed between complex2 dictionaries")
  }

  if complex2.c != ["hello": 42, "<placeholder>": -1] {
    // CHECK-NEXT: in getter
    fatalError("!= failed between complex2 dictionaries")
  }

  // cond: true, initialValue: nil
  _ = UserDefinedInitWithConditionalTest<Int>()
  // CHECK: Wrapper.init($Storage(val: nil))
  // CHECK-NEXT: in getter
  // CHECK-NEXT nil

  // initalValue: nil
  _ = UserDefinedInitWithConditionalTest<[String: any BinaryInteger]>(cond: true)
  // CHECK: Wrapper.init($Storage(val: nil))
  // CHECK-NEXT: in getter
  // CHECK-NEXT: nil

  do {
    let initialValue = (("a", 42), ("b", 0))

    _ = UserDefinedInitWithConditionalTest(cond: true, initialValue: initialValue)
    // CHECK: Wrapper.init($Storage(val: Optional((("a", 42), ("b", 0)))))
    // CHECK-NEXT: in getter
    // CHECK-NEXT: Optional((("a", 42), ("b", 0)))

    _ = UserDefinedInitWithConditionalTest(cond: false, initialValue: initialValue)
    // CHECK: Wrapper.init($Storage(val: nil))
    // CHECK-NEXT: in getter
    // CHECK-NEXT: nil
  }
}

do {
  let test1 = ClassWithConvenienceInit(a: [1, 2, 3])
  // CHECK: in getter
  // CHECK-NEXT: [1, 2, 3]
  // CHECK-NEXT: in getter
  // CHECK-NEXT: <placeholder>
  // CHECK-NEXT: in setter => <modified>
  // CHECK-NEXT: in getter
  // CHECK-NEXT: <modified>

  func test<T>(_ v: T) {
    let test1 = ClassWithConvenienceInit<(Int, String, T)>()
    test1.a = (-1, "ultimate question", v)
    print(test1.a)
  }

  test((a: 1, b: 2.0, c: 3))
  // -> from init(a: T?)
  // CHECK: in getter
  // CHECK-NEXT: nil
  // CHECK-NEXT: in getter
  // CHECK-NEXT: <placeholder>
  // CHECK-NEXT: in setter => <modified>
  // CHECK-NEXT: in getter
  // CHECK-NEXT: <modified>
  // -> from init()
  // CHECK-NEXT: in getter
  // CHECK-NEXT: nil
  // CHECK-NEXT: in getter
  // CHECK-NEXT: <modified>
  // -> from test<T>(_: T)
  // CHECK-NEXT: in setter => Optional((-1, "ultimate question", (a: 1, b: 2.0, c: 3)))
  // CHECK-NEXT: in getter
  // CHECK-NEXT: Optional((-1, "ultimate question", (a: 1, b: 2.0, c: 3)))
}
