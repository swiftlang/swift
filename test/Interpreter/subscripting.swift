// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Check that subscripts and functions named subscript can exist side-by-side
struct Foo {
  subscript() -> String {
    return "instance subscript"
  }
  
  func `subscript`() -> String {
    return "instance func"
  }
  
  static subscript() -> String {
    return "static subscript"
  }
  
  static func `subscript`() -> String {
    return "static func"
  }
}

let f = Foo()
print(f[]) // CHECK: instance subscript
print(f.subscript()) // CHECK: instance func
print(Foo[]) // CHECK: static subscript
print(Foo.subscript()) // CHECK: static func


// SR-7418

protocol P {
  subscript<T : Y>(_: T) -> Int { get set }
}

struct Q : P {
  subscript<T : X>(_ idx: T) -> Int {
    get { return 0 } set { idx.foo() }
  }
}
protocol Y : X {}
protocol X { func foo() }

struct Idx : Y {
  func foo() { print("I survived") }
}

func foo<T : P>(_ t: inout T) {
  t[Idx()] += 1
}

var q = Q()
foo(&q) // CHECK: I survived

protocol PStatic {
  static subscript<T : Y>(_: T) -> Int { get set }
}

struct QStatic : PStatic {
  static subscript<T : X>(_ idx: T) -> Int {
    get { return 0 } set { idx.foo() }
  }
}
func fooStatic<T : PStatic>(_ t: T.Type) {
  t[Idx()] += 1
}

fooStatic(QStatic.self) // CHECK: I survived

