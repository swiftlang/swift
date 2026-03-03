// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -Xfrontend -enable-experimental-feature -Xfrontend ThenStatements -o %t/a.out %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_ThenStatements

func testDeferIf(_ x: Bool) -> Int {
  defer {
    print("defer testDeferIf")
  }
  let x = if x {
    defer {
      print("defer if")
    }
    print("enter if")
    then 0
  } else {
    1
  }
  print("after if")
  return x
}
_ = testDeferIf(true)

// CHECK:      enter if
// CHECK-NEXT: defer if
// CHECK-NEXT: after if
// CHECK-NEXT: defer testDeferIf

func testDeferSwitch(_ x: Bool) -> Int {
  defer {
    print("defer testDeferSwitch")
  }
  let x = switch x {
  case true:
    defer {
      print("defer case true")
    }
    print("enter case true")
    fallthrough
  case false:
    defer {
      print("defer case false")
    }
    print("enter case false")
    then 1
  }
  print("after switch")
  return x
}
_ = testDeferSwitch(true)
// CHECK:      enter case true
// CHECK-NEXT: defer case true
// CHECK-NEXT: enter case false
// CHECK-NEXT: defer case false
// CHECK-NEXT: after switch
// CHECK-NEXT: defer testDeferSwitch

func testFallthrough(_ x: Bool) -> Int {
  var z = 0
  let y: Int
  let x = switch x {
  case true:
    z = 1
    fallthrough
  case false:
    y = 2
    then 3
  }
  return x + y + z
}
print("fallthrough: \(testFallthrough(true))")

// CHECK: fallthrough: 6
