// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

actor class A {
  var delegate: AnyObject?
  @asyncHandler
  func setDelegateToNil() {
    self.delegate = nil
  }
}
func f() {
  let a = A()
  a.setDelegateToNil()
}
f()
print("Hello") // CHECK: Hello
