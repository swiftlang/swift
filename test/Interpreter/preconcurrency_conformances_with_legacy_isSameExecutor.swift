// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-build-swift %t/src/Interface.swift -emit-module -emit-library \
// RUN:    -target %target-cpu-apple-macosx10.15 -swift-version 5 \
// RUN:    -enable-library-evolution \
// RUN:    -module-name Interface \
// RUN:    -o %t/%target-library-name(Interface) \
// RUN:    -emit-module-interface-path %t/Interface.swiftinterface

// RUN: %target-build-swift %t/src/Types.swift -swift-version 5 -emit-module -emit-library -enable-library-evolution -module-name Types -o %t/%target-library-name(Types) \
// RUN:    -target %target-cpu-apple-macosx10.15 \
// RUN:    -I %t -L %t -l Interface \
// RUN:    -emit-module-interface-path %t/Types.swiftinterface \
// RUN:    -Xfrontend -enable-upcoming-feature -Xfrontend DynamicActorIsolation \
// RUN:    -Xfrontend -disable-dynamic-actor-isolation

// RUN: %target-build-swift -I %t -L %t -l Types %t/src/Test1.swift -o %t/test1.out
// RUN: %target-codesign %t/test1.out
// RUN: env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/test1.out 2>&1 | %FileCheck %t/src/Test1.swift

// RUN: %target-build-swift -I %t -L %t -l Types %t/src/Test2.swift -o %t/test2.out
// RUN: %target-codesign %t/test2.out
// RUN: env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/test2.out 2>&1 | %FileCheck %t/src/Test2.swift

// RUN: %target-build-swift -I %t -L %t -l Types %t/src/Test3.swift -o %t/test3.out
// RUN: %target-codesign %t/test3.out
// RUN: env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/test3.out 2>&1 | %FileCheck %t/src/Test3.swift

// RUN: %target-build-swift -I %t -L %t -l Types %t/src/Test4.swift -o %t/test4.out
// RUN: %target-codesign %t/test4.out
// RUN: env SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/test4.out 2>&1 | %FileCheck %t/src/Test4.swift

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DynamicActorIsolation

// rdar://123810657
// UNSUPPORTED: back_deployment_runtime

//--- Interface.swift
public protocol P {
  init()

  var prop: [String] { get set }
  func test() -> Int
}

//--- Types.swift
import Interface

public func runTest<T: P>(_ type: T.Type) async -> Int {
  let v = type.init()
  return v.test()
}

public func runAccessors<T: P>(_ type: T.Type) async -> [String] {
  var v = type.init()
  v.prop = ["a", "b", "c"]
  return v.prop
}

public final class Test : @preconcurrency P {
  @MainActor public var prop: [String] = []
  @MainActor public func test() -> Int { 42 }

  public init() {}
}

public actor ActorTest {
  var x: Int = 0

  public init() {}
}

extension ActorTest : @preconcurrency P {
  public var prop: [String] {
    get { [] }
    set { }
  }

  public func test() -> Int { x }
}

//--- Test1.swift
import Types
print(await runTest(Test.self))
// CHECK-NOT: Incorrect actor executor assumption; Expected MainActor executor

//--- Test2.swift
import Types
print(await runAccessors(Test.self))
// CHECK-NOT: Incorrect actor executor assumption; Expected MainActor executor

//--- Test3.swift
import Types
print(await runTest(ActorTest.self))
// CHECK-NOT: Incorrect actor executor assumption

//--- Test4.swift
import Types
print(await runAccessors(ActorTest.self))
// CHECK-NOT: Incorrect actor executor assumption
