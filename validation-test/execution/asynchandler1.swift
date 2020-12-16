// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

actor class A {
  var delegate: AnyObject?
  @asyncHandler
  func setDelegate(_ delegate: AnyObject) {
    self.delegate = delegate
  }
}
class C {
  init() {
    let a = A()
    a.setDelegate(self)
  }
}
let c = C()
print("Hello") // CHECK: Hello
