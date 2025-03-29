// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.5-abi-triple | %FileCheck %s
// REQUIRES: VENDOR=apple
// UNSUPPORTED: OS=xros

public protocol P<A> {
  associatedtype A
}

public func f<T>(_: T.Type) {}

@available(SwiftStdlib 5.7, *)
public func g<T>(_: T.Type) { f((any P<T>).self) }

// CHECK-LABEL: declare extern_weak ptr @swift_getExtendedExistentialTypeMetadata(ptr, ptr)
