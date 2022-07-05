// RUN: %target-swift-frontend -emit-ir %s -swift-version 5 -import-objc-header %S/Inputs/objc_protocol_sendable.h | %IRGenFileCheck %s

// REQUIRES: objc_interop
// REQUIRES: concurrency

// CHECK: define{{.*}}s22objc_protocol_sendable4test1aySo1AC_tF
public func test(a: A) {
  a.foo()
}
