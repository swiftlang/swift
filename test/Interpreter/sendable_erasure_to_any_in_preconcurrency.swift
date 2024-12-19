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


func test() {
  let c = C()

  print(c.dict.answer())
  // CHECK: 42
  print(c.arr.answer())
  // CHECK: 42

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
}

test()
