// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var MetadataCycleTests = TestSuite("Metadata cycle tests")

// rdar://18448285
class test0_GenericClass<T> {
  func foo() {}
}
class test0_GenericSubclass<T> : test0_GenericClass<test0_GenericSubclass> {}
MetadataCycleTests.test("rdar://18448285") {
  test0_GenericSubclass<Int>().foo()
}

// rdar://18685206
final class test1_Box<T> {
  init(_ value: T) {
    self.value = value
  }
  let value: T
}
enum test1_List<T> {
  case Nil
  case Cons(T, test1_Box<test1_List<T>>)
  var head: T? {
    switch self {
    case .Nil:
      return nil
    case .Cons(let h, _):
      return h
    }
  }
}
MetadataCycleTests.test("rdar://18685206") {
  let x: test1_List<Int> = .Nil
  _ = x.head
}

// rdar://18847269
struct test2_Thunk<T> {}
enum test2_List<T> {
  case Nil
  case Cons(T, test2_Thunk<test2_List>)
}
MetadataCycleTests.test("rdar://18847269") {
  let il0: test2_List<Int> = .Nil
  _ = il0
}

// rdar://18903483
final class test3_Box<T> {
  private let _value : () -> T
  init(_ value : T) {
    self._value = { value }
  }
}
enum test3_List<A> {
  case Nil
  case Cons(A, test3_Box<test3_List<A>>)
}
MetadataCycleTests.test("rdar://18903483") {
  let x : test3_List<Int> = .Nil
  _ = x

  // rdar://19371082
  _ = test3_List.Cons(7, test3_Box(test3_List.Nil)) 
}

// rdar://19320857
struct test4_RoseTree<T> {
  let value: T
  let branches: [test4_RoseTree]
}
MetadataCycleTests.test("rdar://19320857") {
  _ = test4_RoseTree(value: 1, branches: [])
}

runAllTests()
