// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -Xfrontend -enable-experimental-feature -Xfrontend ImplicitLastExprResults -o %t/a.out %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_ImplicitLastExprResults

func testBinding(_ cond: Bool) -> Int {
  let x = if cond {
    let a = 1
    a
  } else {
    let b = 1
    if cond { b } else { b + 1 }
  }
  return x
}
print(testBinding(true))
print(testBinding(false))
// CHECK:      1
// CHECK-NEXT: 2

func testReturn(_ cond: Bool) -> Int {
  ()
  if cond {
    let a = 1
    a
  } else {
    let b = 1
    if cond { b } else { b + 1 }
  }
}

print(testReturn(true))
print(testReturn(false))
// CHECK-NEXT: 1
// CHECK-NEXT: 2

func testDefer(_ cond: Bool) -> Int {
  defer {
    print("defer fn")
  }
  let x = if cond {
    defer {
      print("defer if")
    }
    print("enter if")
    1
  } else {
    print("else branch")
    2
  }
  print("after if")
  return x
}
_ = testDefer(true)
_ = testDefer(false)

// CHECK:      enter if
// CHECK-NEXT: defer if
// CHECK-NEXT: after if
// CHECK-NEXT: defer fn
// CHECK-NEXT: else branch
// CHECK-NEXT: after if
// CHECK-NEXT: defer fn

func testGuard(_ cond: Bool) -> Int {
  switch cond {
  case true:
    let x: Int? = 1
    guard let y = x else { fatalError() }
    y
  case false:
    2
  }
}
print(testGuard(true))
// CHECK-NEXT: 1

func testNestedType(_ cond: Bool) -> Int {
  if cond {
    struct S {
      var x: Int
    }
    S(x: 1).x
  } else {
    2
  }
}
print(testNestedType(true))
// CHECK-NEXT: 1

func testClosure(_ cond: Bool) -> Int {
  let fn = {
    ()
    if cond {
      let a = 1
      a
    } else {
      let b = 1
      if cond { b } else { b + 1 }
    }
  }
  return fn()
}
print(testClosure(true))
print(testClosure(false))
// CHECK-NEXT: 1
// CHECK-NEXT: 2
