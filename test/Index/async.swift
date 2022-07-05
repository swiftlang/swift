// REQUIRES: concurrency

// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

func globalAsyncFunc() async {}
// CHECK: [[@LINE-1]]:6 | function(swift_async)/Swift | globalAsyncFunc() | {{.*}} | Def | rel: 0

struct MyStruct {
  func asyncMethod() async {}
  // CHECK: [[@LINE-1]]:8 | instance-method(swift_async)/Swift | asyncMethod() |
}

class XCTestCase {}
class MyTestCase : XCTestCase {
  func testSomeAsync() async {}
  // CHECK: [[@LINE-1]]:8 | instance-method(test,swift_async)/Swift | testSomeAsync() |
}
