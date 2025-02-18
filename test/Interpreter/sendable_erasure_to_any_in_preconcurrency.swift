// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

class C {
  @preconcurrency var dict: [String : any Sendable] = ["a": 42]
  @preconcurrency var arr: [any Sendable] = [42]
}

extension Dictionary where Key == String, Value == Any {
  func answer() -> Int { self["a"] as! Int }

  subscript(entry e: String) -> (any Sendable)? {
    get { self["Entry#" + e] }
    set { self["Entry#" + e] = newValue }
  }

  var entryB: (any Sendable)? {
    get { self["Entry#B"] }
    set { self["Entry#B"] = newValue }
  }

  mutating func addEntry() {
    self["ultimate question"] = "???"
  }
}

extension Array where Element == Any {
  func answer() -> Int { self.first! as! Int }
}

struct S<T> {
  let v: T
}

struct Test {
  @preconcurrency var data: S<any Sendable>
  @preconcurrency var funcRef: S<([any Sendable]) -> (any Sendable)?> = S(v: { $0.first })
}

func testInOut(_ dict: inout Dictionary<String, Any>) {
  dict["inout"] = "yes"
}

func testInOut(_ arr: inout [Any]) {
  arr.append("inout")
}

func test() {
  var c = C()

  print(c.dict.answer())
  // CHECK: 42
  print(c.arr.answer())
  // CHECK: 42

  print(c.dict[entry: "A"] ?? "no A")
  // CHECK: no A

  // Insert a new value
  c.dict[entry: "A"] = "forty two"

  // Make sure that the dictionary got mutated
  print(c.dict[entry: "A"] ?? "no A")
  // CHECK: forty two

  print(c.dict.entryB ?? "no B")
  // CHECK: no B

  // Insert new value
  c.dict.entryB = (q: "", a: 42)

  print(c.dict.entryB ?? "no B")
  // CHECK: (q: "", a: 42)

  c.dict.addEntry()
  print(c.dict["ultimate question"] ?? "no question")
  // CHECK: ???

  let v1 = Test(data: S(v: 42))
  let v2 = Test(data: S(v: "ultimate question"))

  func expectsAny(_ s: S<Any>) { print(s.v) }

  expectsAny(v1.data)
  // CHECK: 42
  expectsAny(v2.data)
  // CHECK: ultimate question

  func sameType<T>(_ data: S<T>, with: T.Type) -> T {
    data.v
  }

  print(sameType(v1.data, with: Any.self))
  // CHECK: 42
  print(sameType(v2.data, with: Any.self))
  // CHECK: ultimate question

  func expectsFuncAny(_ s: S<([Any]) -> Any?>) {
    print(s.v([42]) ?? 0)
  }

  expectsFuncAny(v1.funcRef)
  // CHECK: 42

  testInOut(&c.dict)
  print(c.dict["inout"] ?? "no")
  // CHECK: yes

  testInOut(&c.arr)
  print(c.arr.contains(where: { $0 as? String == "inout" }))
  // CHECK: true

  var newValues: [Any] = ["other inout"]
  c.arr += newValues // checks implicit inout injection via operator
  print(c.arr.contains(where: { $0 as? String == "other inout" }))
  // CHECK: true
}

test()
