// RUN: %target-swift-frontend -enable-experimental-feature BuiltinMacros -typecheck %s -module-name MacrosTest 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx

// CHECK: #function --> "MacrosTest"
print(#function)

func f(a: Int, b: Int) {
  print(#function, #line, #column)
  // CHECK-NEXT: #function --> "f(a:b:)"
  // CHECK-NEXT: #line --> 8
  // CHECK-NEXT: #column --> 27
}
