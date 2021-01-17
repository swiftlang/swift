// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -enable-library-evolution -enable-experimental-concurrency -emit-module-interface-path %t/Library.swiftinterface -module-name Library %s
// RUN: %FileCheck --check-prefix CHECK-EXTENSION %s <%t/Library.swiftinterface
// RUN: %FileCheck --check-prefix CHECK %s <%t/Library.swiftinterface
// REQUIRES: concurrency

/// This test ensures that, when generating a swiftinterface file,
/// the actor class decl itself is what may conform to the Actor protocol,
/// and not via some extension. The requirement is due to the unique
/// optimizations applied to the implementation of actors.

// CHECK-EXTENSION-NOT: extension {{.+}} : _Concurrency.Actor

// CHECK: actor public class PlainActorClass {
public actor class PlainActorClass {
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

// CHECK: actor public class ExplicitActorClass : _Concurrency.Actor {
public actor class ExplicitActorClass : Actor {
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

// CHECK: actor public class EmptyActorClass {
public actor class EmptyActorClass {}

// CHECK: public protocol Cat : _Concurrency.Actor {
public protocol Cat : Actor {
  func mew()
}

// CHECK: actor public class HouseCat : Library.Cat {
public actor class HouseCat : Cat {
  @asyncHandler public func mew() {}
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}

// CHECK: public protocol ToothyMouth {
public protocol ToothyMouth {
  func chew()
}

// CHECK: actor public class Lion : Library.ToothyMouth, _Concurrency.Actor {
public actor class Lion : ToothyMouth, Actor {
  @asyncHandler public func chew() {}
  @actorIndependent public func enqueue(partialTask: PartialAsyncTask) { }
}