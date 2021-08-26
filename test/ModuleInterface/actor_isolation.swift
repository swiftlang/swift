// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -module-name Test -enable-experimental-concurrency %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %FileCheck %s -check-prefix SYNTHESIZED < %t/Test.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name Test %t/Test.swiftinterface

// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-module-interface-path %t/TestFromModule.swiftinterface -module-name Test -enable-experimental-concurrency
// RUN: %FileCheck %s < %t/TestFromModule.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name Test %t/TestFromModule.swiftinterface

// REQUIRES: concurrency

// CHECK: public actor SomeActor

@available(SwiftStdlib 5.5, *)
public actor SomeActor {
  nonisolated func maine() { }
}

// CHECK: @globalActor public struct SomeGlobalActor

@available(SwiftStdlib 5.5, *)
@globalActor
public struct SomeGlobalActor {
  public static let shared = SomeActor()
}

// CHECK: @{{(Test.)?}}SomeGlobalActor public protocol P1
// CHECK-NEXT: @{{(Test.)?}}SomeGlobalActor func method()

@available(SwiftStdlib 5.5, *)
@SomeGlobalActor
public protocol P1 {
  func method()
}

// CHECK: class C1
// CHECK-NEXT: @{{(Test.)?}}SomeGlobalActor public func method()

@available(SwiftStdlib 5.5, *)
public class C1: P1 {
  public func method() { }
}


@available(SwiftStdlib 5.5, *)
@SomeGlobalActor
public class C2 { }

// CHECK: @{{(Test.)?}}SomeGlobalActor public class C2

@available(SwiftStdlib 5.5, *)
public class C3: C2 { }

// CHECK: public class C4 : Swift.UnsafeSendable

@available(SwiftStdlib 5.5, *)
public class C4: UnsafeSendable { }

// CHECK: public class C5 : @unchecked Swift.Sendable

@available(SwiftStdlib 5.5, *)
public class C5: @unchecked Sendable { }


@available(SwiftStdlib 5.5, *)
public class C6 { }

// CHECK: extension {{(Test.)?}}C6 : @unchecked Swift.Sendable

@available(SwiftStdlib 5.5, *)
extension C6: @unchecked Sendable { }


@available(SwiftStdlib 5.5, *)
public class C7 { }

// CHECK: extension {{(Test.)?}}C7 : Swift.UnsafeSendable

@available(SwiftStdlib 5.5, *)
extension C7: UnsafeSendable { }


@available(SwiftStdlib 5.5, *)
public protocol P2 {
  @SomeGlobalActor func method()
}


// CHECK: class {{(Test.)?}}C8 : {{(Test.)?}}P2 {
@available(SwiftStdlib 5.5, *)
public class C8 : P2 {
  // CHECK: @{{(Test.)?}}SomeGlobalActor public func method()
  public func method() {}
}

// FIXME: Work around a bug where module printing depends on the "synthesized"
// bit in conformances which is not serialized and not present in the textual
// form.

// SYNTHESIZED: extension Test.C2 : Swift.Sendable {}
