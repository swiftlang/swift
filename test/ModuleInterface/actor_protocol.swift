// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck --check-prefix CHECK %s <%t/Library.swiftinterface
// REQUIRES: concurrency

/// This test ensures that, when generating a swiftinterface file,
/// the actor decl itself is what may conform to the Actor protocol,
/// and not via some extension. The requirement is due to the unique
/// optimizations applied to the implementation of actors.

// CHECK-NOT: extension {{.+}} : _Concurrency.Actor

// CHECK: public actor PlainActorClass {
@available(SwiftStdlib 5.1, *)
public actor PlainActorClass {
  nonisolated public func enqueue(_ job: UnownedJob) { }
}

// CHECK: public actor ExplicitActorClass : _Concurrency.Actor {
@available(SwiftStdlib 5.1, *)
public actor ExplicitActorClass : Actor {
  nonisolated public func enqueue(_ job: UnownedJob) { }
}

// CHECK: public actor EmptyActor {
@available(SwiftStdlib 5.1, *)
public actor EmptyActor {}

// CHECK: public actor EmptyActorClass {
@available(SwiftStdlib 5.1, *)
public actor EmptyActorClass {}

// CHECK: public protocol Cat : _Concurrency.Actor {
@available(SwiftStdlib 5.1, *)
public protocol Cat : Actor {
  func mew()
}

// CHECK: public actor HouseCat : Library.Cat {
@available(SwiftStdlib 5.1, *)
public actor HouseCat : Cat {
  nonisolated public func mew() {}
  nonisolated public func enqueue(_ job: UnownedJob) { }
}

// CHECK: public protocol ToothyMouth {
@available(SwiftStdlib 5.1, *)
public protocol ToothyMouth {
  func chew()
}

// CHECK: public actor Lion : Library.ToothyMouth, _Concurrency.Actor {
@available(SwiftStdlib 5.1, *)
public actor Lion : ToothyMouth, Actor {
  nonisolated public func chew() {}
  nonisolated public func enqueue(_ job: UnownedJob) { }
}
