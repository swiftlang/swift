// RUN: %target-run-simple-swift | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: OS=macosx
// REQUIRES: executable_test

protocol P1<P1T> {
  associatedtype P1T

  func printValue()
}

protocol P2<P2T> {
  associatedtype P2T
}

protocol P3<P3T> {
    associatedtype P3T
}

struct Small: P1, P2, P3 {
  typealias P1T = Int
  typealias P2T = Int
  typealias P3T = Int

  var str = "I am Small"

  func printValue() { print(str) }
}

struct Big: P1, P2, P3 {
  typealias P1T = Int
  typealias P2T = Int
  typealias P3T = Int

  var str = "I am Big"
  var str2 = ""
  var str3 = ""

  func printValue() { print(str) }
}

class Class: P1, P2, P3 {
  typealias P1T = Int
  typealias P2T = Int
  typealias P3T = Int

  var str = "I am Class"

  func printValue() { print(str) }
}

func test<T>(_ value: T) {
  var array: [T] = []
  array.append(value)
  for v in array {
    (v as? P1)?.printValue()
  }
}

// CHECK: I am Small
test(Small() as any P1<Int>)
// CHECK: I am Small
test(Small() as any P1<Int> & P2<Int>)
// CHECK: I am Small
test(Small() as any P1<Int> & P2<Int> & P3<Int>)

// CHECK: I am Big
test(Big() as any P1<Int>)
// CHECK: I am Big
test(Big() as any P1<Int> & P2<Int>)
// CHECK: I am Big
test(Big() as any P1<Int> & P2<Int> & P3<Int>)

// CHECK: I am Class
test(Class() as any P1<Int>)
// CHECK: I am Class
test(Class() as any P1<Int> & P2<Int>)
// CHECK: I am Class
test(Class() as any P1<Int> & P2<Int> & P3<Int>)

// CHECK: I am Class
test(Class() as any AnyObject & P1<Int>)
// CHECK: I am Class
test(Class() as any AnyObject & P1<Int> & P2<Int>)
// CHECK: I am Class
test(Class() as any AnyObject & P1<Int> & P2<Int> & P3<Int>)
