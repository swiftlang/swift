// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -enable-library-evolution -enable-experimental-concurrency -enable-experimental-async-handler -emit-module-interface-path %t/Library.swiftinterface -module-name Library %s
// RUN: %FileCheck --check-prefix CHECK-EXTENSION %s <%t/Library.swiftinterface
// RUN: %FileCheck --check-prefix CHECK %s <%t/Library.swiftinterface
// REQUIRES: concurrency

/// This test ensures that, when generating a swiftinterface file,
/// the actor decl itself is what may conform to the Actor protocol,
/// and not via some extension. The requirement is due to the unique
/// optimizations applied to the implementation of actors.

// CHECK-EXTENSION-NOT: extension {{.+}} : _Concurrency.Actor

// CHECK: public actor PlainActorClass {
public actor PlainActorClass {
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

// CHECK: public actor ExplicitActorClass : _Concurrency.Actor {
public actor ExplicitActorClass : Actor {
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

// CHECK: public actor EmptyActor {
public actor EmptyActor {}

// CHECK: actor public class EmptyActorClass {
public actor class EmptyActorClass {}

// CHECK: public protocol Cat : _Concurrency.Actor {
public protocol Cat : Actor {
  func mew()
}

// CHECK: public actor HouseCat : Library.Cat {
public actor HouseCat : Cat {
  @asyncHandler public func mew() {}
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

// CHECK: public protocol ToothyMouth {
public protocol ToothyMouth {
  func chew()
}

// CHECK: public actor Lion : Library.ToothyMouth, _Concurrency.Actor {
public actor Lion : ToothyMouth, Actor {
  @asyncHandler public func chew() {}
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}
