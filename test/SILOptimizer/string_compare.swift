// RUN: %target-swift-frontend -parse-as-library -O -emit-ir  %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -Osize -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// UNSUPPORTED: PTRSIZE=32

// This is an end-to-end test to ensure that the optimizer generates
// optimal code for comparing string literals

// CHECK-LABEL: define {{.*}}test0
// CHECK:      entry:
// CHECK-NEXT:   ret i1 true
// CHECK-NEXT: }
public func test0() -> Bool {
  return "Hello, " + "Swift" == "Hello, Swift"
}

// CHECK-LABEL: define {{.*}}test1
// CHECK:      entry:
// CHECK-NEXT:   ret i64 2
// CHECK-NEXT: }
public func test1() -> Int {
  let str = "a"
  if str == "1" { return 0 }
  if str == "2" { return 1 }
  if str == "a" { return 2 }
  if str == "b" { return 3 }
  return -1
}

// CHECK-LABEL: define {{.*}}test2
// CHECK:      entry:
// CHECK-NEXT:   ret i64 4
// CHECK-NEXT: }
public func test2() -> Int {
  if "a" == "b" { return 1 }

  let x = "abc"
  if x == "123" { return 2 }
  else if x == "456" { return 3 }
  else { return 4 }
}

// CHECK-LABEL: define {{.*}}test3
// CHECK:      entry:
// CHECK-NEXT:   ret i64 2
// CHECK-NEXT: }
public func test3() -> Int {
  let str = "2"

  switch str {
    case "1": return 1
    case "2": return 2
    case "3": return 3
    case "4": return 4
    default : return 0
  }
}

// CHECK-LABEL: define {{.*}}test4
// CHECK:      entry:
// CHECK-NEXT:   ret i64 1
// CHECK-NEXT: }
public func test4() -> Int {
  let str = "1"

  switch str {
    case "1": return 1
    case "2": return 2
    case "3": return 3
    case "4": return 4
    default : return 0
  }
}

// CHECK-LABEL: define {{.*}}test5
// CHECK:      entry:
// CHECK-NEXT:   ret i64 0
// CHECK-NEXT: }
public func test5() -> Int {
  let str = "x"

  switch str {
    case "1": return 1
    case "2": return 2
    case "3": return 3
    case "4": return 4
    default : return 0
  }
}

