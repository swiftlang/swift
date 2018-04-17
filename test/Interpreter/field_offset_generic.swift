// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

public protocol Proto { }

public struct MyImpl: Proto { }

public struct EmptyStruct {}

private struct GenericStruct<T : Proto> {
    var empty: EmptyStruct = EmptyStruct()
    var dummy: Int = 0
    var opt: Optional<T> = nil

    init() {
    }
}

public func test() {
  let s = GenericStruct<MyImpl>()
  assert(s.dummy == 0, "Expecting dummy == 0")
  assert(s.opt == nil, "Expecting opt == nil")
  // CHECK: dummy: 0
  print("dummy: \(s.dummy)")
}

test()
