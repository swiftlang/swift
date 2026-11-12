// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name UserModule -strict-memory-safety
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name UserModule
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix FLAGS %s < %t.swiftinterface

// Interfaces record strict memory safety when enabled via module diagnostic group controls.
// RUN: %target-swift-emit-module-interface(%t.wwarning.swiftinterface) %s -module-name UserModule -Wwarning StrictMemorySafety
// RUN: %target-swift-typecheck-module-from-interface(%t.wwarning.swiftinterface) -module-name UserModule
// RUN: %FileCheck -check-prefix FLAGS %s < %t.wwarning.swiftinterface

// FLAGS: swift-module-flags-ignorable:{{.*}} -strict-memory-safety

// CHECK: @unsafe public func getIntUnsafely() -> Swift::Int
@unsafe public func getIntUnsafely() -> Int { 0 }

public struct UnsafeIterator: @unsafe IteratorProtocol {
  @unsafe public mutating func next() -> Int? { nil }
}

public struct SequenceWithUnsafeIterator: Sequence {
  public init() { }
  public func makeIterator() -> UnsafeIterator { UnsafeIterator() }
}

// CHECK: @inlinable public func useUnsafeCode()
@inlinable public func useUnsafeCode() {
  // CHECK-NOT: unsafe
  print( unsafe getIntUnsafely())

  for unsafe _ in SequenceWithUnsafeIterator() {
    _ = unsafe getIntUnsafely()
  }
}

// CHECK: public protocol P
public protocol P {
  func f()
}

// CHECK: public struct X : @unsafe UserModule::P
public struct X: @unsafe P {
// CHECK:  @unsafe public func f()
  @unsafe public func f() { }
}
