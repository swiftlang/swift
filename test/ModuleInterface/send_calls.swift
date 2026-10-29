// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// An inlinable body which uses 'send' is printed into the module interface,
// so its declaration is guarded by '$SendCalls' for compilers which cannot
// parse 'send'. Bodies which are not printed need no guard

public actor Worker {
  public func tick() {}
}

// CHECK: #if compiler(>=5.3) && $SendCalls
// CHECK-NEXT: @inlinable public func inlinableSend(_ worker: Library::Worker) {
// CHECK-NEXT: send worker.tick()
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@inlinable public func inlinableSend(_ worker: Worker) {
  send worker.tick()
}

// CHECK: #if compiler(>=5.3) && $SendCalls
// CHECK-NEXT: public var inlinableSendInAccessor: Swift::Int {
// CHECK: send worker.tick()
// CHECK: #endif
public let worker = Worker()
public var inlinableSendInAccessor: Int {
  @inlinable get {
    send worker.tick()
    return 0
  }
}

// CHECK-NOT: $SendCalls
// CHECK: public func nonInlinableSend(_ worker: Library::Worker){{$}}
public func nonInlinableSend(_ worker: Worker) {
  send worker.tick()
}
