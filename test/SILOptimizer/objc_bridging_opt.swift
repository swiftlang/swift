// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/objc_bridging_opt/objc.m -c -o %t/objc.o -g
// RUN: %target-build-swift -module-name=test -O %s -import-objc-header %S/Inputs/objc_bridging_opt/objc.h %t/objc.o -o %t/a.out
// RUN: %target-swift-frontend -module-name=test -O -primary-file %s -import-objc-header %S/Inputs/objc_bridging_opt/objc.h -emit-sil | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// CHECK-SIL-LABEL: sil hidden [noinline] @$s4test0A17NonOptionalStringyyF
// CHECK-SIL:     [[F1:%[0-9]+]] = function_ref @returnNSString
// CHECK-SIL:     apply [[F1]]()
// CHECK-SIL-NOT: apply
// CHECK-SIL:     switch_enum
// CHECK-SIL:     [[F2:%[0-9]+]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK-SIL:     apply [[F2]]
// CHECK-SIL-NOT: apply
// CHECK-SIL:     [[F3:%[0-9]+]] = function_ref @useNSString
// CHECK-SIL:     apply [[F3]]
// CHECK-SIL: // end sil function '$s4test0A17NonOptionalStringyyF'
@inline(never)
func testNonOptionalString() {
  useNSString(returnNSString())
}

// CHECK-SIL-LABEL: sil hidden [noinline] @$s4test0A14OptionalStringyySbF
// CHECK-SIL-NOT:    bridge
// CHECK-SIL: // end sil function '$s4test0A14OptionalStringyySbF'
@inline(never)
func testOptionalString(_ some: Bool) {
  useOptNSString(returnOptNSString(some))
}

// CHECK-SIL-LABEL: sil hidden [noinline] @$s4test0a13NonOptionalToC6StringyyF
// CHECK-SIL:     [[F1:%[0-9]+]] = function_ref @returnNSString
// CHECK-SIL:     apply [[F1]]()
// CHECK-SIL-NOT: apply
// CHECK-SIL:     switch_enum
// CHECK-SIL:     [[F2:%[0-9]+]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK-SIL:     apply [[F2]]
// CHECK-SIL-NOT: apply
// CHECK-SIL:     [[F3:%[0-9]+]] = function_ref @useOptNSString
// CHECK-SIL:     apply [[F3]]
// CHECK-SIL: // end sil function '$s4test0a13NonOptionalToC6StringyyF'
@inline(never)
func testNonOptionalToOptionalString() {
  useOptNSString(returnNSString())
}

// CHECK-SIL-LABEL: sil hidden [noinline] @$s4test0a13OptionalToNonB6StringyySbF
// CHECK-SIL-NOT:    bridge
// CHECK-SIL: // end sil function '$s4test0a13OptionalToNonB6StringyySbF'
@inline(never)
func testOptionalToNonOptionalString(_ some: Bool) {
  if let s = returnOptNSString(some) {
    useNSString(s)
  }
}

// CHECK-SIL-LABEL: sil hidden [noinline] @$s4test0A15NonOptionalLoopyySiF
// CHECK-SIL:     [[F1:%[0-9]+]] = function_ref @returnNSString
// CHECK-SIL:     apply [[F1]]()
// CHECK-SIL-NOT: apply
// CHECK-SIL:     switch_enum
// CHECK-SIL:     [[F2:%[0-9]+]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK-SIL:     apply [[F2]]
// CHECK-SIL-NOT: apply
// CHECK-SIL:     [[F3:%[0-9]+]] = function_ref @useNSString
// CHECK-SIL:     apply [[F3]]
// CHECK-SIL: // end sil function '$s4test0A15NonOptionalLoopyySiF'
@inline(never)
func testNonOptionalLoop(_ n: Int) {
  let s = returnNSString()
  for _ in 0..<n {
    useNSString(s)
  }
}

// CHECK-SIL-LABEL: sil hidden [noinline] @$s4test0A12OptionalLoopyySi_SbtF
// CHECK-SIL-NOT:    bridge
// CHECK-SIL: // end sil function '$s4test0A12OptionalLoopyySi_SbtF'
@inline(never)
func testOptionalLoop(_ n: Int, _ some: Bool) {
  let s = returnOptNSString(some)
  for _ in 0..<n {
    useOptNSString(s)
  }
}

@inline(never)
func testNonOptionalNullString() {
  useNSString(returnNullNSString())
}


// CHECK-LABEL: testNonOptionalString
print("testNonOptionalString:")
// CHECK-NEXT: This is an ObjectiveC string!
testNonOptionalString()
// CHECK-NEXT: end
print("end")

// CHECK-LABEL: testOptionalString
print("testOptionalString:")
// CHECK-NEXT: This is an optional ObjectiveC string!
testOptionalString(true)
// CHECK-NEXT: NULL
testOptionalString(false)
// CHECK-NEXT: end
print("end")

// CHECK-LABEL: testNonOptionalToOptionalString
print("testNonOptionalToOptionalString:")
// CHECK-NEXT: This is an ObjectiveC string!
testNonOptionalToOptionalString()
// CHECK-NEXT: end
print("end")

// CHECK-LABEL: testOptionalToNonOptionalString
print("testOptionalToNonOptionalString:")
// CHECK-NEXT: This is an optional ObjectiveC string!
testOptionalToNonOptionalString(true)
testOptionalToNonOptionalString(false)
// CHECK-NEXT: end
print("end")

// CHECK-LABEL: testNonOptionalLoop
print("testNonOptionalLoop:")
// CHECK-NEXT: This is an ObjectiveC string!
// CHECK-NEXT: This is an ObjectiveC string!
// CHECK-NEXT: This is an ObjectiveC string!
testNonOptionalLoop(3)
// CHECK-NEXT: end
print("end")

// CHECK-LABEL: testOptionalLoop
print("testOptionalLoop:")
// CHECK-NEXT: This is an optional ObjectiveC string!
// CHECK-NEXT: This is an optional ObjectiveC string!
// CHECK-NEXT: This is an optional ObjectiveC string!
testOptionalLoop(3, true)
// CHECK-NEXT: NULL
// CHECK-NEXT: NULL
// CHECK-NEXT: NULL
testOptionalLoop(3, false)
// CHECK-NEXT: end
print("end")

// CHECK-LABEL: testNonOptionalNullString
print("testNonOptionalNullString:")
// CHECK-NEXT: <empty>
testNonOptionalNullString()
// CHECK-NEXT: end
print("end")
