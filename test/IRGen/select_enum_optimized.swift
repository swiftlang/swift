// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -O -module-name=test -disable-llvm-optzns -emit-ir | %FileCheck %s

enum NoPayload {
  case E0
  case E1
  case E2
  case E3
}

// Check if the code of a select_num is a simple int cast and not a switch.

// CHECK-LABEL: define {{.*}}selectDirect
// CHECK: %1 = zext i8 %0 to i32
// CHECK: ret i32 %1

@inline(never)
func selectDirect(e: NoPayload) -> Int32 {
  switch e {
  case .E0:
    return 0
  case .E1:
    return 1
  case .E2:
    return 2
  case .E3:
    return 3
  }
}

// CHECK-LABEL: define {{.*}}selectNegOffset
// CHECK: %1 = zext i8 %0 to i32
// CHECK: %2 = add i32 %1, -6
// CHECK: ret i32 %2

@inline(never)
func selectNegOffset(e: NoPayload) -> Int32 {
  switch e {
  case .E0:
    return -6
  case .E1:
    return -5
  case .E2:
    return -4
  case .E3:
    return -3
  }
}

// CHECK-LABEL: define {{.*}}selectPosOffset
// CHECK: %1 = zext i8 %0 to i32
// CHECK: %2 = add i32 %1, 3
// CHECK: ret i32 %2

@inline(never)
func selectPosOffset(e: NoPayload) -> Int32 {
  switch e {
  case .E0:
    return 3
  case .E1:
    return 4
  case .E2:
    return 5
  case .E3:
    return 6
  }
}

// Following functions contain select_enums, which cannot be generated as a
// simple conversion.

// CHECK-LABEL: define {{.*}}selectWithDefault
// CHECK: switch i8
// CHECK: ret

@inline(never)
func selectWithDefault(e: NoPayload) -> Int32 {
  switch e {
  case .E0:
    return 0
  case .E1:
    return 1
  default:
    return 2
  }
}

// CHECK-LABEL: define {{.*}}selectNonContiguous
// CHECK: switch i8
// CHECK: ret

@inline(never)
func selectNonContiguous(e: NoPayload) -> Int32 {
  switch e {
  case .E0:
    return 0
  case .E1:
    return 1
  case .E2:
    return 3
  case .E3:
    return 4
  }
}

var gg : Int32 = 10

// CHECK-LABEL: define {{.*}}selectNonConstant
// CHECK: switch i8
// CHECK: ret

@inline(never)
func selectNonConstant(e: NoPayload) -> Int32 {
  switch e {
  case .E0:
    return 0
  case .E1:
    return 1
  case .E2:
    return gg
  case .E3:
    return 4
  }
}

// CHECK-LABEL: define {{.*}}selectTuple
// CHECK: switch i8
// CHECK: ret

@inline(never)
func selectTuple(e: NoPayload) -> (Int32, Int32) {
  switch e {
  case .E0:
    return (0, 1)
  case .E1:
    return (1, 2)
  case .E2:
    return (2, 3)
  case .E3:
    return (3, 4)
  }
}

// CHECK-LABEL: define {{.*}}selectNonInt
// CHECK: switch i8
// CHECK: ret

@inline(never)
func selectNonInt(e: NoPayload) -> String {
  switch e {
  case .E0:
    return "a"
  case .E1:
    return "ab"
  case .E2:
    return "abc"
  case .E3:
    return "abcd"
  }
}

