// RUN: %target-swift-frontend -emit-ir %s -module-name test -swift-version 5 -sil-verify-all -o /dev/null
// RUN: not %target-swift-frontend -emit-sil %s -module-name test -swift-version 5 -DUSE_BEFORE_INIT 2>&1 | %FileCheck %s
// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/93800

@available(SwiftStdlib 5.1, *)
final class Manager {
  func follow(isolation: isolated (any Actor) = #isolation) {}
}

@available(SwiftStdlib 5.1, *)
actor Plugin {
  let manager: Manager

  init() async {
    manager = Manager()
    manager.follow()
  }

#if USE_BEFORE_INIT
  init(beforeInit: ()) async {
    // CHECK: error: constant 'self.manager' used before being initialized
    // CHECK: [[@LINE+1]]:{{[0-9]+}}: note: expanded code originates here
    Manager().follow()
    manager = Manager()
  }
#endif
}
