// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// CHECK: [[@LINE+1]]:8 | struct/Swift | Int | {{.*}} | Ref | rel: 0
var _: Int { get { return 1 } }

class CrashTest {
  var something = 0
  func returnSelf(_ h: [AnyHashable: Any?]) -> CrashTest {
    return self
  }
  init() { }
}
// CHECK: [[@LINE+1]]:13 | instance-method/Swift | returnSelf
CrashTest().returnSelf(["": 0]).something()
